################################################################
# Install DRA stuff for the MS-Windows build
# Author: Douglas Miles
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__
LIBDIR=$(PLBASE)\library
EXDIR=$(PKGDOC)\examples\dra
DRA=$(LIBDIR)\dra
PL="$(PLHOME)\bin\swipl.exe"

LIBPL= dra.pl
DRAPL= dra.pl
EXAMPLES=


all:		drapl


drapl:
		copy dra.pl

check:		dra.pl
		$(PL) -q -f dra.pl -g test,halt -t 'halt(1)'


!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		@if not exist "$(DRA)\$(NULL)" $(MKDIR) "$(DRA)"
		@for %f in ($(LIBPL)) do \
		    copy "%f" "$(DRA)"
		copy $(DRAPL) "$(LIBDIR)\dra.pl"
		copy README "$(DRA)\README.TXT"
		$(MAKEINDEX)
!ENDIF

html-install:	install-examples
pdf-install:	install-examples

install-examples::
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"

xpce-install::

uninstall::
		@for %f in ($(LIBPL)) do \
		    del "$(DRA)\%f"
		del "$(DRA)\README.TXT"
		del "$(LIBDIR)\dra.pl"
		$(MAKEINDEX)

clean::
		if exist *~ del *~

distclean:	clean


