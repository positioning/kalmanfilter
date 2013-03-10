all: kftrack twosegtrack

kftrack: twosegtrack.tpl yrmonday.cpp azimuth.cpp banner.cpp solstice.cpp
	admb kftrack.tpl yrmonday.cpp azimuth.cpp banner.cpp solstice.cpp

twosegtrack: twosegtrack.tpl yrmonday.cpp azimuth.cpp banner.cpp solstice.cpp
	admb twosegtrack.tpl yrmonday.cpp azimuth.cpp banner.cpp solstice.cpp

clean:
	@rm -vf *.o* kftrack kftrack.htp kftrack.cpp twosegtrack twosegtrack.htp twosegtrack.cpp
