.SUFFIXES: .tpl .o

ifndef ADMB_HOME
  $(error "Error: ADMB_HOME not defined.")
endif

ARCH=-m32
OBJECTS=yrmonday.o azimuth.o banner.o solstice.o

all: kftrack twosegtrack

kftrack: kftrack.o $(OBJECTS)
	LDFLAGS="$(ARCH) $(OBJECTS)" ${ADMB_HOME}/bin/adlink $@

twosegtrack: twosegtrack.o $(OBJECTS)
	LDFLAGS="$(ARCH) $(OBJECTS)" ${ADMB_HOME}/bin/adlink $@

.cpp.o:
	CXXFLAGS=$(ARCH) ${ADMB_HOME}/bin/adcomp $(*F)

.tpl.cpp:
	${ADMB_HOME}/bin/tpl2cpp $*

clean:
	@rm -vf *.o kftrack kftrack.cpp kftrack.htp twosegtrack twosegtrack.cpp twosegtrack.htp
