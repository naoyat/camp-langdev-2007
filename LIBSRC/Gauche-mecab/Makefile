#
# $Id: Makefile.in,v 1.2 2004/07/17 12:37:43 fuyuki Exp $
#

SHELL       = /bin/sh
prefix      = /usr/local
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin
libdir      = ${exec_prefix}/lib


# These may be overridden by make invocators
DESTDIR  =
OPTFLAGS = -fomit-frame-pointer 
CC       = gcc
LIBS     = -L/usr/local/lib/gauche/0.8.11/i686-apple-darwin8.10.1  -lgauche -lm  -lpthread  `mecab-config --libs`
CFLAGS   = -g -O2 -I/usr/local/lib/gauche/0.8.11/include `mecab-config --cflags`
LDFLAGS  =   -bundle -flat_namespace -undefined suppress -o
INSTALL_TYPE = site

# Set by configure
GOSH     = /usr/local/bin/gosh
GAUCHE_CONFIG = /usr/local/bin/gauche-config
OBJEXT   = o
DSOEXT   = so

INSTALL  = /usr/local/bin/gauche-install

MODULE   = mecab

ARCHFILES = $(MODULE).$(DSOEXT)
SCMFILES  = $(MODULE).scm
HEADERS   =

TARGET    = $(ARCHFILES)
OBJS      = $(MODULE)_head.$(OBJEXT) \
            mecab.$(OBJEXT) \
            $(MODULE)_tail.$(OBJEXT)
GENERATED = mecab.c $(MODULE)_head.c $(MODULE)_tail.c
CONFIG_GENERATED = Makefile config.cache config.log config.status \
		   configure.lineno autom4te*.cache Gauche-$(MODULE).gpd

HEADER_INSTALL_DIR  = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)incdir`
SCM_INSTALL_DIR     = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)libdir`
ARCH_INSTALL_DIR    = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)archdir`

all : $(TARGET)

.SUFFIXES: .o .stub

.c.o:
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

.stub.c:
	$(GOSH) genstub $<

$(MODULE).$(DSOEXT): $(OBJS)
	$(CC) $(LDFLAGS) $(MODULE).$(DSOEXT) $(OBJS) $(LIBS)
#	$(CC) -Wl,-rpath -Wl,`gauche-config --sysarchdir` $(LDFLAGS) $(MODULE).$(DSOEXT) $(OBJS) $(LIBS)

$(MODULE)_head.c $(MODULE)_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension $(MODULE)

check : all
	@rm -f test.log
	$(GOSH) -I. test.scm > test.log

install : all
	$(INSTALL) -m 444 -T $(HEADER_INSTALL_DIR) $(HEADERS)
	$(INSTALL) -m 444 -T $(SCM_INSTALL_DIR) $(SCMFILES)
	$(INSTALL) -m 555 -T $(ARCH_INSTALL_DIR) $(ARCHFILES)
	$(INSTALL) -m 444 -T $(SCM_INSTALL_DIR)/.packages Gauche-$(MODULE).gpd

clean :
	rm -rf core $(TARGET) $(OBJS) $(GENERATED) *~ test.log so_locations

distclean : clean
	rm -rf $(CONFIG_GENERATED)

maintainer-clean : clean
	rm -rf $(CONFIG_GENERATED) VERSION aclocal.m4 configure
