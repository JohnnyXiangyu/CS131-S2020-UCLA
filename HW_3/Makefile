files=UnsynchronizedState.java AcmeSafeState.java NullState.java State.java SwapTest.java SynchronizedState.java UnsafeMemory.java
submission=${files} report.pdf

default: ${files}
	javac *.java 
	mv *.class compile/

clean: 
	rm -f compile/*.class 

dist: ${submission}
	rm -f ./submission/*
	cp ${submission} ./submission/
