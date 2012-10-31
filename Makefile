!ifndef ADMB_HOME
error "Error: ADMB_HOME is not defined."
!endif

ADMB_HOME=$(ADMB_HOME)
PATH=$(ADMB_HOME)\bin;$(PATH)

all: build-kfsst build-kftrack build-trackit build-ukfsst

build-kfsst:
	pushd kfsst\kfsst\source & nmake & popd
	copy kfsst\kfsst\source\kfsst.exe

build-kftrack:
	pushd kftrack\src & nmake & popd
	copy kftrack\src\kftrack.exe
	copy kftrack\src\twosegtrack.exe

build-trackit:
	pushd trackit\trackit\inst\admb\src & nmake & popd
	copy trackit\trackit\inst\admb\src\prepro.exe
	copy trackit\trackit\inst\admb\src\ukf.exe

build-ukfsst:
	pushd ukfsst\ukfsst\source & nmake & popd
	copy ukfsst\ukfsst\source\ukfsst.exe
