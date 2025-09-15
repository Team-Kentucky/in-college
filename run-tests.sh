#!/bin/bash

cd bin
line_count=0
success_count=0

#Clearning the testing enviornment
if [ -f "output.txt" ]; then rm "output.txt"; fi
if [ -f "test-output" ]; then rm -rf "test-output"; fi
mkdir -p test-output
printf "[%2d] %-45s [%1s]\n" $line_count "Setting up enviornment" "X"
((line_count++))

# Running Test Suite
for file in ../input-files/*; do
    if [ -f "acct-database.dat" ]; then rm "acct-database.dat"; fi
    if [ -f "input.txt" ]; then rm "input.txt"; fi

    file_no_path=$(basename "$file")
    filename="${file_no_path%.*}"
    output_file="test-output/$filename-output.txt"

    result="F"
    cp $file input.txt
    ./inCollege > $output_file

    expected_output="../output-files/$filename-output.txt"
    if [ -f $expected_output ]
    then
        diff_from_expected=$(diff $output_file $expected_output)
        if [$diff_from_expected == ""]
        then
            result="P"
            ((success_count++))
        fi
    else
        result="X"
    fi

    printf "[%2d] %-45s [%1s]\n" $line_count $filename $result
    if [ $result == "X" ]; then printf "     %s\n" "Expected output file does not exist. Manual verification required"; fi
    ((line_count++))
done
((line_count--))

cp acct-database.dat test-output
cp output.txt test-output

printf "\nTest suite was"; if [ $success_count == $line_count ]; then printf " sucessful. "; else printf " unsuccessful. "; fi
printf "%d/%d tests passed\n" $success_count $line_count

cd ..

