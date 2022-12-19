filepath="examples/qcoutput"

as -o "$filepath/prog.o" "$filepath/prog.s"
ld -macosx_version_min 11.0.0 -o "$filepath/prog" "$filepath/prog.o" -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64 
"./$filepath/prog" > "$filepath/out.txt"