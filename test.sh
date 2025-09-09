#!/bin/bash

cd bin
rm acct-database.dat
rm input.txt
rm output.txt
rm cli-output.txt
cp ../input-files/input1.txt input.txt
./inCollege >> cli-output.txt
rm input.txt
cp ../input-files/input2.txt input.txt
./inCollege >> cli-output.txt
echo "Here is the diff between cli output and logging: "
diff cli-output.txt output.txt
cd ..

