.SUFFIXES: .tpl .obj
CC = /usr/local/gcc-3.4.4/bin/g++
AD_DIR = /usr/local/admb-gcc344
LL = $(CC) -static -s -L$(AD_DIR)/lib
CFLAGS = -static -fpermissive -Wno-deprecated -I. -I${AD_DIR}/include -DUSE_LAPLACE -D__GNUDOS__ -Dlinux -DOPT_LIB -O3
 
ukfsst: ukfsst.cpp ukfsst.obj yrmonday.obj azimuth.obj banner.obj solstice.obj
	$(LL) ukfsst.obj yrmonday.obj banner.obj azimuth.obj solstice.obj -o ukfsst -L${AD_DIR}/lib -ldf1b2o -ladmod -ladt -lado -ldf1b2o -ladmod -ladt -lado

.cpp.obj:
	$(CC) -c $(CFLAGS) -o$*.obj $<

.tpl.cpp:
	$(AD_DIR)/bin/tpl2cpp $*
