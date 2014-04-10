all: ukf prepro

ukf: ukf.tpl 
	admb ukf

prepro: prepro.tpl 
	admb prepro

clean:
	@rm -vf prepro prepro.cpp prepro.htp ukf ukf.htp ukf.cpp *.o*
