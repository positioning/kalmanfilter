!ifndef ADMB_HOME
error "Error: ADMB_HOME is not defined."
!endif

ADMB_HOME=$(ADMB_HOME)
PATH=$(ADMB_HOME)\bin;$(PATH)

all: build-kfsst build-kftrack build-trackit build-ukfsst

build-kfsst:
	pushd deprecated\kfsst\kfsst\source & nmake & popd
	copy deprecated\kfsst\kfsst\source\kfsst.exe

build-kftrack:
	pushd deprecated\kftrack\src & nmake & popd
	copy deprecated\kftrack\src\kftrack.exe
	copy deprecated\kftrack\src\twosegtrack.exe

build-trackit:
	pushd deprecated\trackit\trackit\inst\admb\src & nmake & popd
	copy deprecated\trackit\trackit\inst\admb\src\prepro.exe
	copy deprecated\trackit\trackit\inst\admb\src\ukf.exe

build-ukfsst:
	pushd deprecated\ukfsst\ukfsst\source & nmake & popd
	copy deprecated\ukfsst\ukfsst\source\ukfsst.exe
