To build, run the COBOL: Build Active File task while inCollege.cob is open. (Ctrl+Shift+B)
Then in a terminal, cd to bin and run the output using ./

How to run without any shortcuts:

mkdir -p bin
cobc -x -o bin/inCollege src/inCollege.cob
./bin/inCollege