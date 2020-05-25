code=server.py isc.py
files=report.pdf project.tgz

dist: $(files)
	-mkdir submission/
	cp report.pdf project.tgz submission/

clean: 
	rm report.pdf project.tgz 

gen:
	touch $(files) $(code)

project.tgz: 
	tar -zvf 