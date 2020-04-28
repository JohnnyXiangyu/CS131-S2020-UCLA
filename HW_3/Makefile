files=UnsynchronizedState.java AcmeSafeState.java NullState.java State.java SwapTest.java SynchronizedState.java UnsafeMemory.java report.pdf 

default: ${files}
	javac *.java 

clean: 
	rm -f *.class 

dist: ${files}
	rm -f ./submission/*
	cp ${files} ./submission/
