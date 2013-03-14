all: ukf prepro

ukf: ukf.tpl 
	CXXFLAGS=-fpermissive admb ukf

prepro: prepro.tpl 
	admb prepro

clean:
	@rm -vf prepro ukf ukf.htp ukf.cpp *.o*
