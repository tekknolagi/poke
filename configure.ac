## This is an -*- Autoconf -*- file to be processed with autoconf
## to obtain a configure script.

## Copyright (C) 2016, 2017 Luca Saiu
## Written by Luca Saiu

## This file is part of Jitter.

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
AC_INIT([Jitter],
        m4_esyscmd([build-aux/git-version-gen .tarball-version]),
        [bug-epsilon@gnu.org],
        [jitter],
        [http://ageinghacker.net/jitter])
AC_CONFIG_SRCDIR([NO-WARRANTY])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([build-aux])
AC_REQUIRE_AUX_FILE([trivial-source.c])
AC_CONFIG_HEADERS([config.h jitter/jitter-config.h])


# Autoconf macros for Jitter.
################################################################

# We will not use AC_JITTER here, since we are building Jitter itself rather
# than checking for an installed Jitter; however some of our Autoconf macros,
# which of course are not installed yet, will come in handy.
m4_include([autoconf/jitter.m4])


dnl # CFLAGS hack.
dnl ################################################################

dnl # Using CFLAGS , AM_CFLAGS and target_CFLAGS as recommended in the Automake
dnl # manual [Automake, §Flag Variables Ordering] is sensible in almost any
dnl # situation, but unfortunately not for Jitter.

dnl # Jitter is special in the sense that some generated C files *must* be compiled
dnl # with certain GCC options, and those options are often negative (-fno-OPTION).
dnl # The practice recommended by the Autoconf and Automake authors is to have a
dnl # CFLAGS with reasonable defaults, overridable by the user, which is *appended*
dnl # in the command line after either AM_CFLAGS or target_CFLAGS; that way CFLAGS
dnl # takes priority, and its positive options cannot be overridden.

dnl # For Jitter I adopted a different solution: some reasonable defaults go to
dnl # AM_CFLAGS, and CFLAGS is left empty by default.  The make targets requiring
dnl # special options will define target_CFLAGS as AM_CFLAGS followed by the
dnl # required target-specific options; at build time CFLAGS will be appended to
dnl # target_CFLAGS.

dnl # With this solution the user is still entitled to override the per-target
dnl # options by specifying a CFLAGS at build time (even when doing so might cause
dnl # incorrect code to be generated -- the user is supposed to know what she is
dnl # doing), but at least the default will not overrule the required
dnl # target-specific options.

dnl # Save the original CFLAGS specified at configure time, if any, into
dnl # JITTER_ORIGINAL_CFLAGS .  Then define CFLAGS to be empty.  AM_CFLAGS will be
dnl # defined below, after AC_PROG_CC , just like CFLAGS would be.
dnl if test "x$CFLAGS" != "x"; then
dnl   JITTER_ORIGINAL_CFLAGS="$CFLAGS"
dnl fi
dnl : ${CFLAGS=""}


# Automake initialization.
################################################################

AM_INIT_AUTOMAKE


# Define prefixed aliases for Autoconf macros.
################################################################

# A few macro definitions will end up in an installed header, and their names
# have to be prefixed with JITTER_ so as not to conflict with macros from other
# software.
AC_DEFINE_UNQUOTED([JITTER_PACKAGE_NAME], ["$PACKAGE_NAME"],
                   [The name of the Jitter package])
AC_DEFINE_UNQUOTED([JITTER_PACKAGE_VERSION], ["$PACKAGE_VERSION"],
                   [The Jitter version as a string])
AC_DEFINE_UNQUOTED([JITTER_PACKAGE_BUGREPORT], ["$PACKAGE_BUGREPORT"],
                   [An email address where to report Jitter bugs])


# Check for the C compiler and its features.
################################################################

# Indeed, we rely on a C compiler.
AC_PROG_CC

dnl # This is the following of the CFLAGS hack explained above, setting AM_CFLAGS .
dnl #
dnl # By pre-defining CFLAGS above we have prevented AC_PROG_CC from setting it to a
dnl # reasonable default.  The following code reimplements the documented semantics
dnl # of AC_PROG_CC about the value of CFLAGS [Autoconf, §C Compiler Characteristics],
dnl # but sets JITTER_DEFAULT_CFLAGS instead.
dnl if test "x$JITTER_ORIGINAL_CFLAGS" = "x"; then
dnl   JITTER_DEFAULT_CFLAGS=""
dnl   # The Autoconf documentation does not actually refer to ac_cv_prog_cc_g so its
dnl   # name might change in the future, but this is quite safe; the worst that can
dnl   # happen is that we don't supply -g by default even con configuration where it
dnl   # would work.
dnl   if test "x$ac_cv_prog_cc_g" != "x"; then
dnl     JITTER_DEFAULT_CFLAGS="$JITTER_DEFAULT_CFLAGS -g"
dnl   fi
dnl   if test "x$GCC" != "x"; then
dnl     JITTER_DEFAULT_CFLAGS="$JITTER_DEFAULT_CFLAGS -O2"
dnl   fi
dnl else
dnl   JITTER_DEFAULT_CFLAGS="$JITTER_ORIGINAL_CFLAGS"
dnl fi
dnl # Now we have a resonable JITTER_DEFAULT_CFLAGS .  Use that for AM_CFLAGS, so
dnl # that in the Makefile the default options are *prepended* to Jitter
dnl # runtime-specific options, and CFLAGS as specified by the user is *appended*.
dnl AC_SUBST([AM_CFLAGS], [$JITTER_DEFAULT_CFLAGS])

# Run the early part of the Gnulib initialization.  This is recommended
# right after AC_PROG_CC .
gl_EARLY

# Make sure the compiler supports a recent version of C.  Without this I
# has problem on OpenBSD, which ships with a very old GCC.
AC_PROG_CC_C99

# Check that we can use -c and -o together with the compiler.  I currently don't
# bother supporting compilers lacking this basic feature, but I could if it were
# actually needed.
AC_PROG_CC_C_O
if test "x$NO_MINUS_C_MINUS_O" != "x"; then
  AC_MSG_NOTICE([your C compiler doesn't accept -c and -o together.
                 Such a compiler is currently not supported, but support would
                 be easy to add -- in case this were the only problem])
  AC_MSG_ERROR([please write to $PACKAGE_BUGREPORT if you want to help])
fi

# We do support cross-compiling.
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

# Define a feature CPP macro if we are cross-compiling; also set a shell
# variable holding "yes" or "no" to use in this script.
AC_MSG_CHECKING([if we are cross-compiling])
if test "x$build" = "x$host"; then
   AC_MSG_RESULT([no, native compiling on $host .])
   jitter_cross_compiling=no
else
   AC_DEFINE_UNQUOTED([JITTER_CROSS_COMPILING], [1],
                      [Defined if Jitter was cross-compiled.])
   AC_MSG_RESULT([yes, cross-compiling from $build to $host .])
   jitter_cross_compiling=yes
fi

# Installed shell scripts will have to work with a Bourne shell on the host
# system.  In case of native compilation that is simply @SHELL@ as detected
# here, but for cross-compiled installations we can only guess.
if test "x$jitter_cross_compiling" = "xno"; then
  AC_SUBST([SHELL_OR_BIN_SH], [$SHELL])
else
  AC_SUBST([SHELL_OR_BIN_SH], [/bin/sh])
fi

# We have preprocessed assembly files (for some architectures).
AM_PROG_AS

# Can we actually run on something other than GCC?  Probably, as long as the
# compiler supports a recent version of the C standard.
if test "$GCC" = ""; then
   AC_MSG_WARN([not using GCC.  Trying to go on anyway])
fi

# Checks for C type sizes.
AC_CHECK_SIZEOF([void *])
AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([float])
AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([long double])
AC_TYPE_SIZE_T

# By definition we will take the size of a pointer as the host machine "word
# size".  This information is also used in the test suite, so define a
# substitution as well.
AC_SUBST([SIZEOF_VOID_P], [$ac_cv_sizeof_void_p])


# Check for system characteristics.
################################################################

# I never test on weird systems not supporting shebangs.
AC_SYS_INTERPRETER
if test "x$interpval" != "xyes"; then
   AC_MSG_WARN([your system doesn't support shebang.  Trying to go on anyway])
fi

# I never test on systems limiting file name length to 14 characters.
AC_SYS_LONG_FILE_NAMES
if test "x$ac_cv_sys_long_file_names" != "xyes"; then
   AC_MSG_WARN([file names are limited to 14 characters.  Things may break])
fi


# Check for programs.
################################################################

# mkdir -p is convenient.  It is currently used in the makefile and in
# vm/generate-instructions.in .
AC_PROG_MKDIR_P

# This package contains libraries.  When enabling LTO with static libraries (for
# which setting CFLAGS/LDFLAGS is enough) we need to use the specialized library
# tools which come with GCC.  Autoconf doesn't check for them by default, but
# it's easy to do.  If the specialized tools aren't available we fall back on the
# traditional ones.
AC_CHECK_TOOLS([RANLIB], [gcc-ranlib ranlib])
AC_CHECK_TOOLS([AR], [gcc-ar ar])

# We call objdump at runtime when disassembling.  Even not having it installed
# is not fatal, and the problem is easy to fix anyway after compiling Jitter.
AC_PATH_PROG([JITTER_OBJDUMP], [objdump])
if test "x$jitter_cross_compiling" = "xyes"; then
  AC_DEFINE_UNQUOTED([JITTER_OBJDUMP],
                     ["objdump"],
                     [name of the native objdump utility on the host system])
elif test "$JITTER_OBJDUMP" = ""; then
  AC_MSG_WARN([no native objdump found. \
Assuming it will be installed and called "objdump", if disassembling is \
desired])
  AC_DEFINE_UNQUOTED([JITTER_OBJDUMP],
                     ["objdump"],
                     [name of the native objdump utility (not actually found)])
else
  AC_DEFINE_UNQUOTED([JITTER_OBJDUMP],
                     ["$JITTER_OBJDUMP"],
                     [name of the native objdump utility])
fi

# We also check for a cross-objdump, which is very convenient when debugging and
# can be invoked from cross-compiled programs run on the build machine thru
# qemu-user.
AC_CHECK_TOOLS([JITTER_CROSS_OBJDUMP], [objdump])
if test "x$JITTER_CROSS_OBJDUMP" = "x"; then
  AC_MSG_WARN([no cross-objdump found])
else
  AC_DEFINE_UNQUOTED([JITTER_CROSS_OBJDUMP],
                     ["$JITTER_CROSS_OBJDUMP"],
                     [name of the cross-objdump utility from GNU binutils])
fi

# Check for tr, using by configure.
AC_PATH_PROG([JITTER_TR], [tr])
if test "x$JITTER_TR" = "x"; then
  AC_MSG_ERROR([you need the Unix tr utility])
fi

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

# Check for Texinfo commands.  In practice makeinfo is more important than
# texi2dvi, as Info is the default format.
AC_PATH_PROG([JITTER_MAKEINFO], [makeinfo])
if test "x$JITTER_MAKEINFO" = "x"; then
  if test -e "$srcdir/doc/jitter.info"; then
    AC_MSG_WARN([you will need GNU Texinfo to regenerate the Info \
documentation, in case you change the Texinfo source])
  else
    AC_MSG_ERROR([you need GNU Texinfo to generate the Info documentation, if \
you compile from git; release tarballs contain the documentation \
pre-generated])
  fi
fi
AC_PATH_PROG([JITTER_TEXI2DVI], [texi2dvi])
if test "x$JITTER_TEXI2DVI" = "x"; then
  AC_MSG_WARN([you will need texi2dvi, from GNU Texinfo, if you want to \
generate DVI, PostScript or PDF documentation])
fi

# Check for GNU help2man.  That is not required for the user: pre-generated man
# pages are included in the distribution.
AC_CHECK_PROG(HAS_HELP2MAN, help2man, yes, no)
if test "x$HAS_HELP2MAN" != "xyes"; then
  jitter_has_help2man="no"
  if test -e "$srcdir/jitterc/jitterc.1"; then
    AC_MSG_WARN([You need GNU help2man if you want to rebuild manual pages])
  else
    AC_MSG_WARN([You need GNU help2man to build manual pages if you compile
                 from git and you are not content with just man page stubs])
  fi
else
  jitter_has_help2man="yes"
fi
AM_MISSING_PROG(HELP2MAN, help2man)


# Check for programs only used in the test suite.
################################################################

# JITTER_CHECK_TESTSUITE_PROGS
# ----------------------------
# Check the availability of a program only used for the test suite in the
# predefined binary paths.  The first program is the name of the substitution to
# define (empty if the program is not found, otherwise its pathname); the second
# argument is a list of possible basenames.  If the program is not found then
# the first argument is added to the shell variable , and a warning message is
# printed.
# jitter_test_suite_missing_progs.
# Example:
# JITTER_CHECK_TESTSUITE_PROG([JITTER_CMP], [some-other-weird-cmp cmp])
AC_DEFUN([JITTER_CHECK_TESTSUITE_PROGS],
  [AC_PATH_PROGS([$1], [$2])
   if test "x$$1" = "x"; then
     AC_MSG_WARN([none of {$2} found.  Cannot run the test suite])
     jitter_test_suite_missing_progs="$jitter_test_suite_missing_progs $1"
   fi])

# Check for optional programs only used by the test suite.
JITTER_CHECK_TESTSUITE_PROGS([JITTER_AWK], [gawk mawk nawk awk])
JITTER_CHECK_TESTSUITE_PROGS([JITTER_CMP], [cmp])
JITTER_CHECK_TESTSUITE_PROGS([JITTER_BASENAME], [basename])
JITTER_CHECK_TESTSUITE_PROGS([JITTER_DIRNAME], [dirname])
#JITTER_CHECK_TESTSUITE_PROGS([JITTER_SEQ], [seq]) # not currently used.

# Some other programs we can live without, still useful (but not mandatory) for
# the test suite:
AC_PATH_PROGS([JITTER_TIMEOUT], [timeout])


# Utility functions for configure.
################################################################

# Output stdin, translating every lower-case ASCII character into its upper-case
# version, and - into _; every other character is left unchanged.
jitter_lowercase_characters='abcdefghijklmnopqrstuvwxyz-'
jitter_uppercase_characters='ABCDEFGHIJKLMNOPQRSTUVWXYZ_'
jitter_to_upper ()
{
  $JITTER_TR "$jitter_lowercase_characters" "$jitter_uppercase_characters"
}

# Perform a translation which is the reverse of the one in jitter_to_upper .
jitter_to_lower ()
{
  $JITTER_TR "$jitter_uppercase_characters" "$jitter_lowercase_characters"
}

# If the given $CC command-line option is supported (its actual effect is not
# tested) then append the option, after a space, to the named shell veriable.
# Two arguments: the name of a shell variable to possibly modify, and a
# candidate $CC option.
jitter_check_cc_option ()
{
  jitter_flag_variable_name="$1"
  jitter_compiler_option="$2"

  # We cache the result of an option being supported (as "yes" or "no") in a
  # shell variable named after the option.  Read the appropriate variable for
  # $jitter_compiler_option into jitter_cached_value .
  jitter_variable_name="jitter_cc_option_supported_"AS_TR_SH($jitter_compiler_option)
  AS_VAR_COPY([jitter_cached_value], [$jitter_variable_name])

  # If we lack a cached value then perform the test, and set jitter_result ;
  # otherwise set jitter_result from the cached value.
  if test "x$jitter_cached_value" = "x"; then
    AC_MSG_CHECKING([if $CC supports the option $jitter_compiler_option])
    if "$CC" \
         -c -o test.o \
         "$jitter_compiler_option" \
         "$srcdir/build-aux/trivial-source.c" \
           > /dev/null \
           2> /dev/null; then
      jitter_result=yes
    else
      jitter_result=no
    fi
    AS_VAR_SET([$jitter_variable_name], [$jitter_result])
    AC_MSG_RESULT([$jitter_result])
  else
    jitter_result="$jitter_cached_value"
  fi

  # Now $jitter_result is either "yes" or "no".  Use it to decide whether to
  # append the option to the flag variable.
  if test "x$jitter_result" = "xyes"; then
    AS_VAR_COPY([jitter_flag_variable_old_value], [$jitter_flag_variable_name])
    AS_VAR_SET([$jitter_flag_variable_name],
               "$jitter_flag_variable_old_value $jitter_compiler_option")
  fi
}

# Like jitter_check_cc_option , but working on zero or more $CC candidate options.
# One or more arguments: the first is the name of a shell variable, as in the
# first argument of jitter_check_cc_option ; all the other arguments are individual
# candidate $CC options.
jitter_check_cc_options ()
{
  jitter_flag_variable_name="$1"
  shift

  for jitter_compiler_option in $@; do
    jitter_check_cc_option "$jitter_flag_variable_name" "$jitter_compiler_option"
  done
}


# Libtool support.
################################################################

# Initialize Libtool.
LT_INIT

# Update the libtool script if it becomes out of date.
AC_SUBST([LIBTOOL_DEPS])


# Initialize Gnulib.
################################################################

gl_INIT


# Emacs Lisp support.
################################################################

# Check for the Emacs Lisp installation path.
AM_PATH_LISPDIR


# Check for libraries.
################################################################


# Check for headers.
################################################################


# Check for specific functions in the C library.
################################################################

# Check for functions letting me know the page size.
AC_CHECK_FUNCS([sysconf getpagesize])

# Check for functions letting me allocate executable space.
AC_CHECK_FUNCS([mmap64 mmap mremap munmap])

# Check for getrlimit/setrlimit and alarm.
AC_CHECK_FUNCS([getrlimit setrlimit alarm])


# System-dependent configuration.
################################################################

# Define a few macros describing the host and the build systems.
AC_DEFINE_UNQUOTED([JITTER_HOST_CPU], ["$host_cpu"],
                   [The host CPU architecture, as per the GNU convention.])
AC_DEFINE_UNQUOTED([JITTER_HOST_VENDOR], ["$host_vendor"],
                   [The host vendor, as per the GNU convention.])
AC_DEFINE_UNQUOTED([JITTER_HOST_OS], ["$host_os"],
                   [The host operating system, as per the GNU convention.])
AC_DEFINE_UNQUOTED([JITTER_HOST_TRIPLET], ["$host"],
                   [The host system triplet, as per the GNU convention.])
AC_DEFINE_UNQUOTED([JITTER_BUILD_CPU], ["$build_cpu"],
                   [The build CPU architecture, as per the GNU convention.])
AC_DEFINE_UNQUOTED([JITTER_BUILD_VENDOR], ["$build_vendor"],
                   [The build vendor, as per the GNU convention.])
AC_DEFINE_UNQUOTED([JITTER_BUILD_OS], ["$build_os"],
                   [The build operating system, as per the GNU convention.])
AC_DEFINE_UNQUOTED([JITTER_BUILD_TRIPLET], ["$build"],
                   [The build system triplet, as per the GNU convention.])

# Define a feature macro if the host system is GNU.  This is useful for
# conditionalizing on GNU libc features.
if echo "$host_os" | grep -q gnu; then
  AC_DEFINE_UNQUOTED([JITTER_HOST_OS_IS_GNU], [1],
                     [Is the host operating system GNU?])
fi

# Check endianness.
AC_C_BIGENDIAN

# Use a command-line option to explicitly disable assembly support.  I've needed
# this for a version of FreeBSD, which shipped with a very old Gas.
# This is used to compute the default $jitter_has_host_assembly ; a check below
# will decide if support for the host machine is actually available.
AC_MSG_CHECKING([if assembly support is (potentially) enabled])
AC_ARG_ENABLE([assembly],
              AS_HELP_STRING([--disable-assembly],
              [disable assembly, even if available for host machine]),
              jitter_has_host_assembly="$enableval",
              jitter_has_host_assembly="yes")
if    test "x$jitter_has_host_assembly" != "xyes" \
   && test "x$jitter_has_host_assembly" != "xno"; then
  AC_MSG_ERROR([wrong option argument: assembly support not enabled or disabled])
fi
AC_MSG_RESULT([$jitter_has_host_assembly])


# Check whether we have assembly support for the host architecture.
# For the architectures we know, also define sensible options for objdump --
# some of these are necessary, others a matter of preference.  When the
# architecture is unknown some default is guessed.
#
# At this point jitter_has_host_assembly is already defined to "yes" or
# "no".  If assembly support is not found a "yes" will turn into a "no" after
# this check, but a "no" will not turn into a "yes": the option above is
# conceived to *disable* assembly support when available.
AC_MSG_CHECKING([if we have assembly support for $host])
AS_CASE([$host_cpu],
        [aarch64*],    [#JITTER_ASSEMBLY_SUBDIRECTORY=aarch64
                        jitter_has_host_assembly=no;
                        jitter_objdump_options="--architecture=aarch64 --disassembler-options=reg-names-raw"],
        [arm*],        [#JITTER_ASSEMBLY_SUBDIRECTORY=arm
                        jitter_has_host_assembly=no;
                        jitter_objdump_options="--architecture=arm --disassembler-options=reg-names-raw"],
        [i?86],        [#JITTER_ASSEMBLY_SUBDIRECTORY=i386
                        jitter_has_host_assembly=no;
                        jitter_objdump_options="--architecture=i386 --disassembler-options=att,suffix"],
        [mips|mipsel], [JITTER_ASSEMBLY_SUBDIRECTORY=mips;
                        jitter_objdump_options="--architecture=mips:isa32 --disassembler-options=no-aliases,reg-names=numeric"],
        [mips64|mips64el],
                       [#JITTER_ASSEMBLY_SUBDIRECTORY=mips;
                        jitter_has_host_assembly=no;
                        jitter_objdump_options="--architecture= --disassembler-options=no-aliases,reg-names=numeric"],
        [powerpc64|powerpc64le],
                       [#JITTER_ASSEMBLY_SUBDIRECTORY=powerpc
                        jitter_has_host_assembly=no;
                        jitter_objdump_options="--architecture=powerpc:common64"],
        [ppc|powerpc|powerpcle],
                       [JITTER_ASSEMBLY_SUBDIRECTORY=powerpc],
        [sh4*],        [JITTER_ASSEMBLY_SUBDIRECTORY=sh
                        jitter_objdump_options="--architecture=sh4a --disassembler-options=no-aliases"],
        [sparc*],      [JITTER_ASSEMBLY_SUBDIRECTORY=sparc
                        jitter_objdump_options="--architecture=sparc:v9b --disassembler-options=no-aliases"],
        [x86_64],      [JITTER_ASSEMBLY_SUBDIRECTORY=x86_64;
                        jitter_objdump_options="--architecture=i386:x86-64 --disassembler-options=x86-64,att,suffix"],
                       # Default case.  Notice that, as a fallback case, we call
                       # objdump with "--architecture=" , without actually
                       # specifying an architecture name.
                       # Using a default architecture might not be completely
                       # reliable in the case of ISA extensions, but appears to
                       # work well in practice; in any case this is a fallback
                       # case: it is easy to provide a sensible default for
                       # supported architectures by adding cases above.
                       # There is no need for objdump to guess endianness, as
                       # the information is always provided as a separate option
                       # by the Jittery program, which knows the correct value
                       # from configure.
                       [jitter_has_host_assembly=no;
                        jitter_objdump_options="--architecture= --disassembler-options=no-aliases"])
if test "x$jitter_objdump_options" = "x"; then
  jitter_objdump_options="--architecture=$host_cpu"
fi
AC_DEFINE_UNQUOTED([JITTER_OBJDUMP_OPTIONS],
                   ["$jitter_objdump_options"],
                   [architecture-specific options for objdump])
if test "x$jitter_has_host_assembly" = "xno"; then
  AC_MSG_RESULT([no])
else
  AC_MSG_RESULT([yes: $JITTER_ASSEMBLY_SUBDIRECTORY])
fi


# Check if assembly support exists for the host machine.
################################################################

# The Automake conditional JITTER_HAS_ASSEMBLY will be true iff there is support for
# the host machine.  If so JITTER_ASSEMBLY_SUBDIRECTORY will be AC_SUBST'ed to be
# the subdirectory holding configuration-specific files.
if test "x$jitter_has_host_assembly" = "xyes"; then
   AC_SUBST([JITTER_ASSEMBLY_SUBDIRECTORY])
   AC_DEFINE_UNQUOTED([JITTER_HAS_ASSEMBLY], [yes],
                      [Define if we have assembly for the host architecture.])
   AC_DEFINE_UNQUOTED([JITTER_ASSEMBLY_SUBDIRECTORY], ["$JITTER_ASSEMBLY_SUBDIRECTORY"],
                      [the subdirectory containing assembly files.])
fi
AM_CONDITIONAL([JITTER_HAS_ASSEMBLY],
               [test "x$jitter_has_host_assembly" = "xyes"])


# M4sh machinery.
################################################################

# For portability and for better factoring I generate shell scripts with M4sh.
# The actual source files are ".m4sh.in" scripts, to be preprocessed by autom4te
# into ".in" scripts, for aclocal to perform @-substitutions into ordinary
# scripts (no file extension).
# The first step of this double translation relies on autom4te, which is not
# required on the user's machine; therefore I distribute ".in" scripts, but
# the user needs autom4te if she wants to regenerate them.
AC_CHECK_PROG(HAS_AUTOM4TE, autom4te, yes, no)
if test "x$HAS_AUTOM4TE" != "xyes"; then
  AC_MSG_WARN([You need autom4te, coming from GNU Autoconf, if you want to
               rebuild scripts from their M4sh source])
fi
AM_MISSING_PROG(AUTOM4TE, autom4te)
AM_CONDITIONAL([JITTER_HAS_AUTOM4TE],
               [test "x$HAS_AUTOM4TE" = "xyes"])


# Emulator support for running cross-compilied binaries.
################################################################

# We optionally support qemu-user emulation for running cross-compiled binaries
# for the host on the build system; other user binary emulators might work as
# well.
# This is particularly useful for running the test suite when cross-compiling.
# The option argument is the emulator command line included options, to be
# followed by the emulated program with its own options: for example
# "qemu-ppc -L /my-cross-path/powerpc-unknown-linux-gnu/sysroot" .
AC_MSG_CHECKING([if host emulator support is used])
AC_ARG_WITH([emulator],
            AS_HELP_STRING([--with-emulator="EMULATOR WITH OPTIONS"],
            [use an emulator running cross-compiled binaries,
             including the test suite, on the build platform: default no]),
            jitter_with_emulator="$withval",
            jitter_with_emulator="")
if    test "x$jitter_with_emulator" = "x" \
   || test "x$jitter_with_emulator" = "xno"; then
  jitter_with_emulator=""
  AC_MSG_RESULT([no])
else
  AC_MSG_RESULT([yes, $jitter_with_emulator])
fi
AC_SUBST([JITTER_EMULATOR], $jitter_with_emulator)


# Can we actually rebuild man pages?
################################################################

# GNU help2man can be made to work even when cross-compiling, as long as there
# is a usable emulator.  If not we have to do without man pages.
if    test "x$jitter_cross_compiling" = "xyes" \
   && test "x$jitter_with_emulator" = "x"; then
   jitter_can_rebuild_man_pages="no"
else
   jitter_can_rebuild_man_pages="yes"
fi
if test "x$jitter_has_help2man" != "xyes"; then
   jitter_can_rebuild_man_pages="no"
fi
AM_CONDITIONAL([JITTER_CAN_REBUILD_MAN_PAGES],
               [test "x$jitter_can_rebuild_man_pages" = "xyes"])


# Other configuration options.
################################################################

# We optionally support valgrind in the test suite.  First use a configure
# option to override the default.
AC_MSG_CHECKING([if Valgrind is supported])
AC_ARG_WITH([valgrind],
            AS_HELP_STRING([--with-valgrind[=VALGRINDNAME]],
                           [use Valgrind for the test suite (default:
                            detected)]),
            jitter_with_valgrind="$withval",
            jitter_with_valgrind="detect")
# At this point $jitter_with_valgrind may be defined as "yes" , "no" ,
# "detect" , or the program name.

# Decide whether to actually enable valgrind.  Right now we disable
# valgrind when cross-compilation or an emulator is enabled at the
# same time.
if test "x$jitter_cross_compiling" = "xyes"; then
  AC_MSG_RESULT([no, disabled because we are cross-compiling])
  AC_SUBST([JITTER_VALGRIND], [])
elif ! test "x$JITTER_EMULATOR" = "x"; then
  AC_MSG_RESULT([no, disabled because an emulator is also used])
  AC_SUBST([JITTER_VALGRIND], [])
elif test "x$jitter_with_valgrind" = "xno"; then
  AC_MSG_RESULT([no, disabled by configure option])
  AC_SUBST([JITTER_VALGRIND], [])
elif    test "x$jitter_with_valgrind" = "xdetect" \
     || test "x$jitter_with_valgrind" = "xyes"; then
  AC_PATH_PROG([JITTER_VALGRIND], [valgrind])
  if test "x$JITTER_VALGRIND" = "x"; then
    AC_MSG_RESULT([no, not found])
    if test "x$jitter_with_valgrind" = "xyes"; then
      AC_MSG_ERROR([Valgrind requested but not found])
    fi
  else
    AC_MSG_RESULT([yes, found as $JITTER_VALGRIND])
  fi
else
  AC_MSG_RESULT([yes, defined by configure option as $jitter_with_valgrind])
  AC_SUBST([JITTER_VALGRIND], [$jitter_with_valgrind])
fi

# Decide whether we can run the test suite...
AC_MSG_CHECKING([if we can run the test suite])
# By default we can run the test suite if we have all the needed utilites
# *and* either we are not cross-compiling, or if an emulator is enabled...
if    test "x$jitter_test_suite_missing_progs" = "x" \
   && (   test "x$jitter_cross_compiling" = "xno" \
       || ! test "x$JITTER_EMULATOR" = "x"); then
  default=yes
else
  default=no
fi
# ... But this default can be overridden with a configure option.
AC_ARG_ENABLE([test-suite],
              AS_HELP_STRING([--enable-test-suite],
                             [enable the test suite (default: detected)]),
              jitter_enable_test_suite="$enableval",
              jitter_enable_test_suite="$default")
if    ! test "x$jitter_enable_test_suite" = "xyes" \
   && ! test "x$jitter_enable_test_suite" = "xno"; then
   AC_MSG_ERROR([invalid option argument for --enable-test-suite])
fi

# Now we know if the test suite is enabled or not.  Define a substitution and
# an Automake conditional about it.
AC_SUBST([JITTER_ENABLE_TEST_SUITE], [$jitter_enable_test_suite])
AC_MSG_RESULT([$JITTER_ENABLE_TEST_SUITE])
AM_CONDITIONAL([JITTER_ENABLE_TEST_SUITE],
               [test "x$JITTER_ENABLE_TEST_SUITE" = "xyes"])

# If the test suite was enabled despite a missing dependency, print a warning.
if    test "x$jitter_enable_test_suite" = "xyes" \
   && ! test "x$jitter_test_suite_missing_progs" = "x"; then
   AC_MSG_WARN([enabling the test suite despite missing programs, as \
requested.  You can expect some spurious failures])
fi

# Print a warning about skipped test cases if we can run the test suite but
# Valgrind is disabled.
if    test "x$JITTER_CAN_RUN_TESTSUITE" = "xyes" \
   && test "x$JITTER_VALGRIND" = "x"; then
  AC_MSG_WARN([the test suite will skip Valgrind test cases])
fi

# Check whether each dispatching model is available and enabled, in order of
# increasing preference.  The last found to be enabled is also considered the
# best.
jitter_enabled_dispatch_models=""
jitter_best_dispatch_model=""

# Is switch dispatching enabled?  If so define a CPP feature macro.  Also define
# a substitution and an Automake conditional.
AC_MSG_CHECKING([if switch dispatching is enabled])
# FIXME: replace help text below with "default=yes" when I actually implement
# switch dispatching.
default=no--unimplemented
AC_ARG_ENABLE([dispatch-switch],
              AS_HELP_STRING([--enable-dispatch-switch],
              [enable switch dispatching: default no (unimplemented)]),
              jitter_enable_dispatch_switch="$enableval",
              jitter_enable_dispatch_switch="$default")
if test "x$jitter_enable_dispatch_switch" = "xyes"; then
  jitter_best_dispatch_model="switch"
  jitter_enabled_dispatch_models="$jitter_enabled_dispatch_models switch"
  AC_DEFINE_UNQUOTED([JITTER_ENABLE_DISPATCH_SWITCH], 1,
                     [enable switch dispatching])
  AC_SUBST([JITTER_ENABLE_DISPATCH_SWITCH], [1])
else
  AC_SUBST([JITTER_ENABLE_DISPATCH_SWITCH], [])
  true
fi
AC_MSG_RESULT([$jitter_enable_dispatch_switch])
AM_CONDITIONAL([JITTER_ENABLE_DISPATCH_SWITCH],
               [test "x$jitter_enable_dispatch_switch" = "xyes"])


# Is direct-threading enabled?  If so define a CPP feature macro.  Also define
# a substitution and an Automake conditional.
AC_MSG_CHECKING([if direct-threading dispatch is enabled])
default=$(test "x$GCC" = "xyes" && echo yes)
AC_ARG_ENABLE([dispatch-direct-threading],
              AS_HELP_STRING([--enable-dispatch-direct-threading],
              [enable direct-threading dispatch: default yes if and
only if GCC or a compiler claiming to be GCC is used]),
              jitter_enable_dispatch_direct_threading="$enableval",
              jitter_enable_dispatch_direct_threading="$default")
if test "x$jitter_enable_dispatch_direct_threading" = "xyes"; then
  jitter_best_dispatch_model="direct-threading"
  jitter_enabled_dispatch_models="$jitter_enabled_dispatch_models direct-threading"
  AC_DEFINE_UNQUOTED([JITTER_ENABLE_DISPATCH_DIRECT_THREADING], 1,
                     [enabled direct-threading dispatch])
  AC_SUBST([JITTER_ENABLE_DISPATCH_DIRECT_THREADING], [1])
else
  AC_SUBST([JITTER_ENABLE_DISPATCH_DIRECT_THREADING], [])
  true
fi
AC_MSG_RESULT([$jitter_enable_dispatch_direct_threading])
AM_CONDITIONAL([JITTER_ENABLE_DISPATCH_DIRECT_THREADING],
               [test "x$jitter_enable_dispatch_direct_threading" = "xyes"])

# Is minimal-threading enabled?  If so define a CPP macro.  Also define
# a substitution and an Automake conditional.
AC_MSG_CHECKING([if minimal-threading dispatch is enabled])
default=$(test "x$GCC" = "xyes" && echo yes || echo no)
AC_ARG_ENABLE([dispatch-minimal-threading],
              AS_HELP_STRING([--enable-dispatch-minimal-threading],
              [enable minimal threading dispatch: default yes if and
only if GCC or a compiler claiming to be GCC is used]),
              jitter_enable_dispatch_minimal_threading="$enableval",
              jitter_enable_dispatch_minimal_threading="$default")
if test "x$jitter_enable_dispatch_minimal_threading" = "xyes"; then
  jitter_best_dispatch_model="minimal-threading"
  jitter_enabled_dispatch_models="$jitter_enabled_dispatch_models minimal-threading"
  AC_DEFINE_UNQUOTED([JITTER_ENABLE_DISPATCH_MINIMAL_THREADING], 1,
                     [enabled minimal-threading dispatch])
  AC_SUBST([JITTER_ENABLE_DISPATCH_MINIMAL_THREADING], [1])
else
  AC_SUBST([JITTER_ENABLE_DISPATCH_MINIMAL_THREADING], [])
  true
fi
AC_MSG_RESULT([$jitter_enable_dispatch_minimal_threading])
AM_CONDITIONAL([JITTER_ENABLE_DISPATCH_MINIMAL_THREADING],
               [test "x$jitter_enable_dispatch_minimal_threading" = "xyes"])

# Is no-threading enabled?  If so define a CPP macro.  Also define a
# substitution and an Automake conditional.
AC_MSG_CHECKING([if no-threading dispatch is enabled])
if test "x$GCC" = "xyes" && test "x$jitter_has_host_assembly" = "xyes"; then
  default=yes
else
  default=no
fi
AC_ARG_ENABLE([dispatch-no-threading],
              AS_HELP_STRING([--enable-dispatch-no-threading],
              [enable no-threading dispatch: default yes if and
only if GCC or a compiler claiming to be GCC is used and the
host architecture has assembly support]),
              jitter_enable_dispatch_no_threading="$enableval",
              jitter_enable_dispatch_no_threading="$default")
if test "x$jitter_enable_dispatch_no_threading" = "xyes"; then
  jitter_best_dispatch_model="no-threading"
  jitter_enabled_dispatch_models="$jitter_enabled_dispatch_models no-threading"
  AC_DEFINE_UNQUOTED([JITTER_ENABLE_DISPATCH_NO_THREADING], 1,
                     [enabled no-threading dispatch])
  AC_SUBST([JITTER_ENABLE_DISPATCH_NO_THREADING], [1])
else
  AC_SUBST([JITTER_ENABLE_DISPATCH_NO_THREADING], [])
  true
fi
AC_MSG_RESULT([$jitter_enable_dispatch_no_threading])
AM_CONDITIONAL([JITTER_ENABLE_DISPATCH_NO_THREADING],
               [test "x$jitter_enable_dispatch_no_threading" = "xyes"])

# AC_SUBST the list of all the enabled dispatching models, and the best one;
# these don't use quotes are are meant for jitter-config and shell scripts, not
# for C code.
AC_SUBST([JITTER_ENABLED_DISPATCHES],
         [$jitter_enabled_dispatch_models])
AC_SUBST([JITTER_BEST_DISPATCH],
         [$jitter_best_dispatch_model])

# Define a substitution holding the best dispatching model name, and a a CPP
# feature macro named after the best dispatching model.
# We also need an Autoheader template for every possible CPP "best-distpatching"
# macro name, as the actual name is computed with a shell expansion and
# autoheader cannot discover every possibility.
AH_TEMPLATE([JITTER_BEST_DISPATCHING_MODEL_IS_SWITCH],
            [Defined if switch is the best enabled dispatch.])
AH_TEMPLATE([JITTER_BEST_DISPATCHING_MODEL_IS_DIRECT_THREADING],
            [Defined if direct-threading is the best enabled dispatch.])
AH_TEMPLATE([JITTER_BEST_DISPATCHING_MODEL_IS_MINIMAL_THREADING],
            [Defined if minimal-threading is the best enabled dispatch.])
AH_TEMPLATE([JITTER_BEST_DISPATCHING_MODEL_IS_NO_THREADING],
            [Defined if no-threading is the best enabled dispatch.])
if test "x$jitter_best_dispatch_model" = "x"; then
  AC_MSG_ERROR([no dispatchnig model is enabled])
else
  AC_MSG_NOTICE([enabled dispatching models are $jitter_enabled_dispatch_models .])
  AC_MSG_NOTICE([the best enabled dispatching model is \"$jitter_best_dispatch_model\" .])
  AC_SUBST([JITTER_BEST_DISPATCHING_MODEL], [$jitter_best_dispatch_model])
  JITTER_BEST_DISPATCH_MODEL=$(echo $jitter_best_dispatch_model | jitter_to_upper)
  AC_DEFINE_UNQUOTED([JITTER_BEST_DISPATCHING_MODEL_NAME_LOWER],
                     ["$jitter_best_dispatch_model"],
                     [The name of the best dispatching model, lower-case.])
  AC_DEFINE_UNQUOTED([JITTER_BEST_DISPATCHING_MODEL_NAME_UPPER],
                     ["$JITTER_BEST_DISPATCH_MODEL"],
                     [The name of the best dispatching model, upper-case with
                      '-' replaced by '_'.])
  AC_DEFINE_UNQUOTED([JITTER_BEST_DISPATCHING_MODEL_IS_$JITTER_BEST_DISPATCH_MODEL],
                     [1],
                     [Defined if $jitter_best_dispatch_model is the best
                      enabled dispatching model.])
fi


# Installation paths, relative to installation prefixes.
################################################################

# According to the GNU Coding Standards installation prefixes can be decided
# very late, at make time; therefore there are no AC_SUBST or AC_DEFINE calls
# fixing complete paths here.  However we can establish once and for all some
# subdirectories, relative to prefixes we don't know yet.
AC_SUBST([JITTER_FLAG_SUBDIRECTORY],
         [lib/$PACKAGE_TARNAME/$PACKAGE_VERSION/flags])
AC_SUBST([JITTER_TEMPLATE_SUBDIRECTORY],
         [$PACKAGE_TARNAME/$PACKAGE_VERSION/templates])


# Flags.
################################################################

# FIXME: why can't I access $ac_abs_top_builddir ?
jitter_abs_top_builddir="$(pwd)"

# Generate flag files in $builddir/flags/$dispatch_model , for every enabled
# dispatch_model .  Cleanup at the beginning, cd back to $builddir at the end.
rm -rf flags
mkdir flags
for dispatch_model in $jitter_enabled_dispatch_models; do
  DISPATCH_MODEL="$(echo $dispatch_model | jitter_to_upper)"

  # Generate flags for $dispatch_model
  mkdir "flags/$dispatch_model"
  AC_MSG_NOTICE([generating flags for dispatching model \"$dispatch_model\"...])
  JITTER_FLAGS_CFLAGS=""
  jitter_check_cc_options JITTER_FLAGS_CFLAGS -O2 -fomit-frame-pointer
  jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-reorder-blocks
  jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-crossjumping
  jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-thread-jumps
  jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-tree-tail-merge
  jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-isolate-erroneous-paths-dereference
  jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-split-paths
#  jitter_check_cc_options JITTER_FLAGS_CFLAGS -frename-registers
#  jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-plt
  if test "$dispatch_model" != "switch"; then
    jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-gcse
  fi
  case "$dispatch_model" in
    minimal-threading|no-threading)
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fPIC
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-align-loops
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-align-jumps
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-align-labels
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-jump-tables
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-tree-switch-conversion
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -flive-range-shrinkage
#      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fsched-pressure
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-ipa-icf
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-ipa-cp
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-ipa-cp-clone
      jitter_check_cc_options JITTER_FLAGS_CFLAGS -fno-gcse
      ;;
  esac
  case "$host_cpu" in
    x86_64)
      if    test "$dispatch_model" = "minimal-threading" \
         || test "$dispatch_model" = "no-threading" ; then
        jitter_check_cc_options JITTER_FLAGS_CFLAGS -mcmodel=large
      fi
      ;;
    *)
#      AC_MSG_ERROR([unknown dispatch model: error in configure.ac])
      true
      ;;
  esac

  JITTER_FLAGS_CPPFLAGS="-DJITTER_DISPATCH_$DISPATCH_MODEL=1"

  JITTER_FLAGS_LDADD="-ljitter--$dispatch_model"

  # Search for Jitter runtime libraries in the right place.
  JITTER_FLAGS_LDFLAGS="-L${libdir}"
  export JITTER_FLAGS_LDFLAGS

  echo "$CC"                    > flags/$dispatch_model/CC
  echo "$JITTER_FLAGS_CPPFLAGS" > flags/$dispatch_model/CPPFLAGS
  echo "$JITTER_FLAGS_CFLAGS"   > flags/$dispatch_model/CFLAGS
  echo "$JITTER_FLAGS_LDADD"    > flags/$dispatch_model/LDADD
  echo "$JITTER_FLAGS_LDFLAGS"  > flags/$dispatch_model/LDFLAGS

  # Also AC_SUBST flag variables for the best dispatching model, if the best
  # happens to be the one we're handling now.
  if test "x$jitter_best_dispatch_model" = "x$dispatch_model"; then
    AC_SUBST([JITTER_CFLAGS],
             [$(cat flags/$dispatch_model/CFLAGS)])
    AC_SUBST([JITTER_CPPFLAGS],
             [$(cat flags/$dispatch_model/CPPFLAGS)])
    AC_SUBST([JITTER_LDADD],
             [$(cat flags/$dispatch_model/LDADD)])
    AC_SUBST([JITTER_LDFLAGS],
             [$(cat flags/$dispatch_model/LDFLAGS)])
#    AC_SUBST([JITTER_MAKE_FLAGS],
#             '$(MAKEFLAGS) CFLAGS="$(AM_CFLAGS) $(JITTER_CFLAGS)" CPPFLAGS="$(AM_CPPFLAGS) $(JITTER_CPPFLAGS)" LDFLAGS="$(AM_LDFLAGS) $(JITTER_LDFLAGS)" LDADD="$(JITTER_LDADD)"') # There is no AM_LDADD
  fi
done

# AC_SUBST flag variables for every dispatching model.  FIXME: the first
# argument of AC_SUBST must be literal , therefore a shell loop doesn't work
# here, but it shouldn't be hard with m4 macros.
if test "x$jitter_enable_dispatch_switch" = "xyes"; then
  AC_SUBST([JITTER_SWITCH_CFLAGS],
           [$(cat flags/switch/CFLAGS)])
  AC_SUBST([JITTER_SWITCH_CPPFLAGS],
           [$(cat flags/switch/CPPFLAGS)])
  AC_SUBST([JITTER_SWITCH_LDADD],
           [$(cat flags/switch/LDADD)])
  AC_SUBST([JITTER_SWITCH_LDFLAGS],
           [$(cat flags/switch/LDFLAGS)])
#  AC_SUBST([JITTER_SWITCH_MAKE_FLAGS],
#           '$(MAKEFLAGS) CFLAGS="$(AM_CFLAGS) $(JITTER_SWITCH_CFLAGS)" CPPFLAGS="$(AM_CPPFLAGS) $(JITTER_SWITCH_CPPFLAGS)" LDFLAGS="$(AM_LDFLAGS) $(JITTER_SWITCH_LDFLAGS)" LDADD="$(JITTER_SWITCH_LDADD)"') # There is no AM_LDADD
fi
if test "x$jitter_enable_dispatch_direct_threading" = "xyes"; then
  AC_SUBST([JITTER_DIRECT_THREADING_CFLAGS],
           [$(cat flags/direct-threading/CFLAGS)])
  AC_SUBST([JITTER_DIRECT_THREADING_CPPFLAGS],
           [$(cat flags/direct-threading/CPPFLAGS)])
  AC_SUBST([JITTER_DIRECT_THREADING_LDADD],
           [$(cat flags/direct-threading/LDADD)])
  AC_SUBST([JITTER_DIRECT_THREADING_LDFLAGS],
           [$(cat flags/direct-threading/LDFLAGS)])
#  AC_SUBST([JITTER_DIRECT_THREADING_MAKE_FLAGS],
#           '$(MAKEFLAGS) CFLAGS="$(AM_CFLAGS) $(JITTER_DIRECT_THREADING_CFLAGS)" CPPFLAGS="$(AM_CPPFLAGS) $(JITTER_DIRECT_THREADING_CPPFLAGS)" LDFLAGS="$(AM_LDFLAGS) $(JITTER_DIRECT_THREADING_LDFLAGS)" LDADD="$(JITTER_DIRECT_THREADING_LDADD)"') # There is no AM_LDADD
fi
if test "x$jitter_enable_dispatch_minimal_threading" = "xyes"; then
  AC_SUBST([JITTER_MINIMAL_THREADING_CFLAGS],
           [$(cat flags/minimal-threading/CFLAGS)])
  AC_SUBST([JITTER_MINIMAL_THREADING_CPPFLAGS],
           [$(cat flags/minimal-threading/CPPFLAGS)])
  AC_SUBST([JITTER_MINIMAL_THREADING_LDADD],
           [$(cat flags/minimal-threading/LDADD)])
  AC_SUBST([JITTER_MINIMAL_THREADING_LDFLAGS],
           [$(cat flags/minimal-threading/LDFLAGS)])
#  AC_SUBST([JITTER_MINIMAL_THREADING_MAKE_FLAGS],
#           '$(MAKEFLAGS) CFLAGS="$(AM_CFLAGS) $(JITTER_MINIMAL_THREADING_CFLAGS)" CPPFLAGS="$(AM_CPPFLAGS) $(JITTER_MINIMAL_THREADING_CPPFLAGS)" LDFLAGS="$(AM_LDFLAGS) $(JITTER_MINIMAL_THREADING_LDFLAGS)" LDADD="$(JITTER_MINIMAL_THREADING_LDADD)"') # There is no AM_LDADD
fi
if test "x$jitter_enable_dispatch_no_threading" = "xyes"; then
  AC_SUBST([JITTER_NO_THREADING_CFLAGS],
           [$(cat flags/no-threading/CFLAGS)])
  AC_SUBST([JITTER_NO_THREADING_CPPFLAGS],
           [$(cat flags/no-threading/CPPFLAGS)])
  AC_SUBST([JITTER_NO_THREADING_LDADD],
           [$(cat flags/no-threading/LDADD)])
  AC_SUBST([JITTER_NO_THREADING_LDFLAGS],
           [$(cat flags/no-threading/LDFLAGS)])
#  AC_SUBST([JITTER_NO_THREADING_MAKE_FLAGS],
#           '$(MAKEFLAGS) CFLAGS="$(AM_CFLAGS) $(JITTER_NO_THREADING_CFLAGS)" CPPFLAGS="$(AM_CPPFLAGS) $(JITTER_NO_THREADING_CPPFLAGS)" LDFLAGS="$(AM_LDFLAGS) $(JITTER_NO_THREADING_LDFLAGS)" LDADD="$(JITTER_NO_THREADING_LDADD)"') # There is no AM_LDADD
fi

# AC_SUBST a set of LDFLAGS to be prepended to the ordinary ones, only useful
# for linking from an uninstlaled build directory.  @JITTER_UNINSTALLED_LDFLAGS@
# must always come before other *_LDFLAGS variables, since the first -L option
# takes precedence.
AC_SUBST([JITTER_UNINSTALLED_LDFLAGS],
         [-L$jitter_abs_top_builddir/lib])


# Temporary hacks, to investigate later.
################################################################

# My (cross-) GCC crashes on PowerPC if I specify -fpic or -fPIC thru
# the function attribute "optimize" -- however these options should be
# irrelevant on PowerPC, since according to the GCC documentation GCC
# *always* generates PIC code on PowerPC.
#
# Sparc seems to have the same problem.
#
# FIXME: investigate and report the bug, if it's GCC's fault and not
# mine.  For the time being, this workaround is sufficient.
case "$host_cpu" in
  powerpc*|ppc*|sparc*)
  AC_MSG_WARN([working around a GCC problem which might actually be my fault...])
  AC_DEFINE_UNQUOTED([DASHFPIC_MAKES_GCC_SEGFAULT], [1],
                     [Work-around a GCC crash which might be a bug or my fault]);;
esac


# Debugging support.
################################################################

# We generate readable assembly files for debugging; on some architectures I
# prefer an alternative non-default format, which I find more readable.  These
# options are passed to the compiler only when generating textual assembly as
# a make target.
case "$host_cpu" in
  powerpc*|ppc*)
    # I like register names to be visually distinct from numeric immediates.
    AC_SUBST([JITTER_MACHINE_SPECIFIC_READABLE_ASSEMBLY_FLAGS], [-mregnames]);;
esac

# Look for the indent utility, and remember whether it is available in an
# Automake conditional.  This is only used for debugging targets.
AC_PATH_PROG([JITTER_INDENT], [indent])
if test "x$JITTER_INDENT" = "x"; then
  AC_MSG_WARN([no indent utility found. \
You will not be able to build nice-looking preprocessed C files for \
debugging.  You can still generate preprocessed files, but they will \
not look as nice])
fi
# FIXME: use AC_PATH_PROGS_FEATURE_CHECK to check for indent command line
# argument support.
AM_CONDITIONAL([JITTER_HAVE_INDENT], ! test "x$JITTER_INDENT" = "x")


# Test suite.
################################################################

# Use the TAP interface to exchange data with tests.  This requires a script,
# distributed with Automake.
AC_REQUIRE_AUX_FILE([tap-driver.sh])

dnl FIXME: this would be needed for autotest, but I'm not completely sure that
dnl the complexity is justified right now.  Anyway I want to try it.
dnl AC_CONFIG_TESTDIR(tests)

dnl AC_CONFIG_FILES([tests/frontend/parse.sh],
dnl                 [chmod +x tests/frontend/parse.sh])
dnl AC_CONFIG_FILES([tests/frontend/idempotence.sh],
dnl                 [chmod +x tests/frontend/idempotence.sh])
dnl AC_CONFIG_FILES([tests/frontend/twice-defined-label.sh],
dnl                 [chmod +x tests/frontend/twice-defined-label.sh])
dnl AC_CONFIG_FILES([tests/frontend/undefined-label.sh],
dnl                 [chmod +x tests/frontend/undefined-label.sh])
dnl AC_CONFIG_FILES([tests/frontend/wrong-parameter.sh],
dnl                 [chmod +x tests/frontend/wrong-parameter.sh])

# Generate test suite scripts.
AC_CONFIG_FILES([tests/utility]) # This should not be made executable.
AC_CONFIG_FILES([tests/elementary/init-fini.test],
                [chmod +x tests/elementary/init-fini.test])
AC_CONFIG_FILES([tests/uninspired/fundamental.test],
                [chmod +x tests/uninspired/fundamental.test])
AC_CONFIG_FILES([tests/uninspired/programs.test],
                [chmod +x tests/uninspired/programs.test])

# Generate convenience emulator script.
AC_CONFIG_FILES([scripts/emulator],
                [chmod +x scripts/emulator])

# Generate convenience disassembly highlighter for ANSI terminals.
AC_CONFIG_FILES([scripts/highlight-disassembly],
                [chmod +x scripts/highlight-disassembly])

# Generate jitter-config script.
AC_CONFIG_FILES([scripts/jitter-config],
                [chmod +x scripts/jitter-config])


# ChangeLog generation.
################################################################

# Define an Automake conditional telling whether we're building from git.
# This is useful to decide whether to rebuild the ChangeLog file on dist.
AM_CONDITIONAL([JITTER_BUILDING_FROM_GIT],
               [test -d "$srcdir/.git"])


# Output.
################################################################

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([gnulib-local/Makefile])

AC_OUTPUT
