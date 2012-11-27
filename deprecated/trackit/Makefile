SDIR = trackit/inst/admb/src/
BDIR = trackit/inst/admb/bin/
SVN = $(shell svn info | grep Revision: | grep -o [1-9].*)
DATE = $(shell date +%Y-%m-%d)
UNAME = $(shell cd web/trackit/; ls *.gz; cd ../..)
VNAME = $(shell echo $(UNAME) | sed s/trackit_// | sed s/.tar.gz//)

clean: 
	rm -rf trackit.Rcheck
	rm -f trackit*.tar.gz
	rm -f trackit/inst/admb/bin/ukf
	rm -f trackit/inst/admb/bin/prepro
	rm -f trackit/inst/pdf/trackit-ref.pdf
	rm -f web/trackit/trackit.zip
	rm -f web/trackit/trackit*.tar.gz


compile: prepro ukf

prepro: $(BDIR)prepro

ukf: $(BDIR)ukf

upload: 
#	cd doc; pdflatex onepage; pdflatex onepage; pdf2web onepage; cd wwwonepage; cp -r big small ../../web/trackit; cd ../..
#	cd doc; rm -rf *.aux *.log *.pdf *.ps *~ wwwonepage
	sed 's/\(trackit_[0-9].[0-9]-[0-9]*.tar.gz\)/$(UNAME)/g' web/trackit/install > tmp 
	mv tmp web/trackit/install  
	sed 's/\([0-9]*-[0-9]*-[0-9]*\)/$(DATE)/' web/trackit/index.html > tmp
	mv tmp web/trackit/index.html  
	sed 's/\([0-9]\.[0-9]-[0-9]*\)/$(VNAME)/' web/trackit/index.html > tmp
	mv tmp web/trackit/index.html  
	sed 's/maxpage=[0-9]*/maxpage=$(shell ls web/trackit/small/| wc -w)/' web/trackit/index.html > tmp
	mv tmp web/trackit/index.html  
	cd web; rsync -aHvz --delete trackit -e ssh nielsens@nielsensweb.org:~/public_html/

pack: ukf prepro 
	sed 's/\(Ver.*-\)\([1-9].*\)/\1$(SVN)/' trackit/DESCRIPTION > tmp  
	sed 's/\(Date: \)\(.*\)/\1$(DATE)/' tmp > trackit/DESCRIPTION 
	rm -f tmp 
	echo "deltat<-read.table('trackit/inst/admb/src/deltaT.dat', skip=4, head=FALSE);\
              colnames(deltat)<-c('day', 'month', 'year', 'DTminusUT');\
	      write.table(deltat, file='trackit/data/deltat.tab', row.names=FALSE)" | R --vanilla
	R CMD check trackit
	rm -f trackit/inst/pdf/trackit-ref.pdf
	echo 'system("R CMD Rd2dvi --no-preview --pdf --title=\"Reference manual for the trackit package\" -o trackit/inst/pdf/trackit-ref.pdf  trackit")' | R --vanilla 
	R CMD build trackit
	rm -f trackit/data/deltat.tab
	rm -f web/trackit/trackit*.tar.gz
	mv trackit*.tar.gz web/trackit
	echo "unlink('trackit.Rcheck/trackit/admb/bin/ukf'); \
              unlink('trackit.Rcheck/trackit/admb/bin/prepro'); \
              strs<-readLines('trackit.Rcheck/trackit/DESCRIPTION');\
              idx<-grep('Built',strs);\
              pts<-strsplit(strs[11],';')[[1]];\
              strs[idx]<-paste(pts[1],'',pts[3],'windows', sep='; ');\
              writeLines(strs[-idx], 'trackit.Rcheck/trackit/DESCRIPTION');\
              setwd('trackit.Rcheck');\
              system('zip -r ../trackit.zip trackit');\
              setwd('..')" | R --vanilla
	mv trackit.zip web/trackit         

trackit/inst/admb/bin/prepro: $(SDIR)prepro.tpl 
	rm -f $@	
	cp $(SDIR)prepro.tpl ./prepro.tpl
	cp $(SDIR)newverify.h ./newverify.h 
	cp $(SDIR)Makefile ./makeadmb
	make -f makeadmb prepro
	mv prepro $(BDIR)prepro
	rm -f prepro.cpp  prepro.htp newverify.h prepro.o prepro.tpl makeadmb

trackit/inst/admb/bin/ukf: $(SDIR)basics.h $(SDIR)chol.h $(SDIR)nLogNormal.h $(SDIR)vspline.cpp $(SDIR)ukf.tpl $(SDIR)quadmap.h
	rm -f $@
	cp $(SDIR)basics.h . 
	cp $(SDIR)chol.h . 
	cp $(SDIR)nLogNormal.h .
	cp $(SDIR)vspline.cpp .
	cp $(SDIR)ukf.tpl . 
	cp $(SDIR)quadmap.h . 
	cp $(SDIR)newverify.h . 
	cp $(SDIR)Makefile ./makeadmb 
	make -f makeadmb ukf
	mv ukf $(BDIR)ukf
	rm -f basics.h chol.h nLogNormal.h vspline.cpp ukf.tpl newverify.h ukf.cpp ukf.htp ukf.o quadmap.h makeadmb


