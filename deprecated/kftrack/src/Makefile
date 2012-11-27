SRCS=yrmonday.cpp azimuth.cpp banner.cpp solstice.cpp

all: kftrack twosegtrack

kftrack: kftrack.tpl $(SRCS)
	admb kftrack.tpl $(SRCS)

twosegtrack: twosegtrack.tpl $(SRCS)
	admb twosegtrack.tpl $(SRCS)
