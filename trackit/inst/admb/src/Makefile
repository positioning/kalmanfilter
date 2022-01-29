all: clean
	$(MAKE) ../ukf
	$(MAKE) ../prepro

../ukf: ukf.tpl 
	admb -o $@ $^

../prepro: prepro.tpl 
	admb -o $@ $^

clean:
	@rm -vf ../prepro prepro.cpp prepro.htp ../ukf ukf.htp ukf.cpp *.o*
