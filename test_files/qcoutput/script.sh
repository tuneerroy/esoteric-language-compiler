as -o prog.o prog.s
ld -macosx_version_min 11.0.0 -o prog prog.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64 
./prog > out.txt