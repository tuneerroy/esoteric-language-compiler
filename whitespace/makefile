test: test.o
	ld -macosx_version_min 11.0.0 -o test test.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64 

test.o: test.s
	as -o test.o test.s

clean:
	rm *.o
# HelloWorld: HelloWorld.o
# 	ld -macosx_version_min 11.0.0 -o HelloWorld HelloWorld.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64 

# HelloWorld.o: HelloWorld.s
# 	as -o HelloWorld.o HelloWorld.s


ld -macosx_version_min 11.0.0 -o helloworld helloworld.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64 
