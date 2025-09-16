#!/bin/bash

printf "InCollege automated testing :)\n"
printf "%4s %-45s %s\n" "#" "Test Name" "Result"

line_count=0
success_count=0

#Clearning the testing enviornment
result="S"
missing_binary=false
if [ -d "bin" ]; then cd bin;
else result="F"; fi
if [ "$result" == "S" ]
then
    if [ -f "inCollege" ]
    then :
    else result="F"; missing_binary=true; fi
    if [ -f "output.txt" ]; then rm "output.txt"; fi
    if [ -d "test-output" ]; then rm -rf "test-output"; fi
    mkdir -p test-output
fi

printf "[%2d] %-45s [%1s]\n" $line_count "Setting up enviornment" $result
((line_count++))

if [ "$result" == "S" ];
then
    # Running Test Suite
    for file in ../input-files/*.txt; do
        if [ -f "acct-database.dat" ]; then rm "acct-database.dat"; fi
        if [ -f "input.txt" ]; then rm "input.txt"; fi

        file_no_path=$(basename "$file")
        filename="${file_no_path%.*}"
        output_file="test-output/$filename-output.txt"

        # Find associated testing database if it exists
        database="../input-files/$filename-database.dat"
        if [ -f "$database" ]; then
            cp $database "acct-database.dat"
        fi

        result="F"
        cp $file input.txt
        ./inCollege > $output_file

        cat $output_file >> "test-output/cli-output.txt"

        expected_output="../output-files/$filename-output.txt"
        if [ -f $expected_output ]
        then
            if [ "$(cat $output_file | grep "$(cat $expected_output)" )" != "" ]
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

    # Check if everything displayed to cli was properly logged
    diff_from_cli=$(diff "test-output/cli-output.txt" "output.txt")

    if [ "$diff_from_cli" == "" ]
    then
        result="P"
        ((success_count++))
    else
        result="F"
    fi
    printf "[%2d] %-45s [%1s]\n" $line_count "Command line matches output log" $result

    cp output.txt test-output

    printf "\nTest suite was"; if [ $success_count == $line_count ]; then printf " sucessful. "; else printf " unsuccessful. "; fi
    printf "%d/%d tests passed\n" $success_count $line_count
elif [ $missing_binary == true ]
then
    printf "     %s\n" "Program binary does not exist. Have you built it? (Try: make)"
elif [ $missing_binary == false ]
then
    printf "     %s\n" "Bin folder does not exist. Have you built the program? (Try: make)";
fi

cd ..

