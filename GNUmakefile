all: build-kfsst build-kftrack build-trackit build-ukfsst

build-kfsst:
	make --directory=kfsst/kfsst/source

build-kftrack:
	make --directory=kftrack/inst/admb/src

build-trackit:
	make --directory=trackit/inst/admb/src

build-ukfsst:
	make --directory=ukfsst/inst/admb/src

clean:
	make --directory=kfsst/kfsst/source clean
	make --directory=kftrack/inst/admb/src clean
	make --directory=trackit/inst/admb/src clean
	make --directory=ukfsst/inst/admb/src clean
