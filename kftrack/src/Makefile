SRCS=yrmonday.cpp azimuth.cpp banner.cpp solstice.cpp

all: kftrack twosegtrack

kftrack: kftrack.tpl $(SRCS)
	admb $(CXXFLAGS) kftrack.tpl $(SRCS)

twosegtrack: twosegtrack.tpl $(SRCS)
	admb $(CXXFLAGS) twosegtrack.tpl $(SRCS)
