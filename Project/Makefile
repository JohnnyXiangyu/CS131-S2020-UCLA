code=server.py isc.py logger.py dev_flags.py api.py report.pdf
files= project.tgz

dist: $(files)
	-mkdir submission/
	cp $(files) submission/

clean: 
	rm $(files) report.pdf
	rm -rf submission/*

gen:
	touch $(files) $(code)

report.pdf: report/report.pdf
	cp report/report.pdf ./report.pdf

project.tgz: $(code)
	tar -czvf project.tgz $(code) 