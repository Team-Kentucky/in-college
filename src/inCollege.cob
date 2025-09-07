IDENTIFICATION DIVISION.
program-id. INCOLLEGE.


*>###################################################################
ENVIRONMENT DIVISION.

input-output section.
file-control.
       select input-file assign to 'input.txt'
           organization is line sequential
           file status is input-file-status.

       select output-file assign to 'output.txt'
           organization is line sequential
           file status is output-file-status.


*>###################################################################
DATA DIVISION.

file section.
*>-----readInputLine variables-----
fd input-file.
01 input-buffer pic x(100). *> Need to decide on reasonable size for lines

*>-----outputLine variables-----
fd output-file.
01 output-line pic x(100).

working-storage section.
*>-----readInputLine variables-----
01 input-file-status pic xx.

*>-----outputLine variables-----
01 output-buffer pic x(100).
01 output-file-status pic xx.

local-storage section.


*>###################################################################
PROCEDURE DIVISION.
main.
       move "test output line" to output-buffer.
       perform outputLine

       open input input-file.

       perform 4 times
           perform readInputLine
           move input-buffer to output-buffer
           perform outputLine
       end-perform.

       close input-file.
       stop run.


*> Paragraph: readInputLine
*> Purpose:   Reads in the next line of the input file
*> Input:     None
*> Output:    input-buffer - Line from the file
*> User is responsible for opening and closing the input file when using this
readInputLine.
       if input-file-status = "00"
           read input-file
               at end
                   *> Input-buffer is stale now, so set to spaces
                   move spaces to input-buffer
                   *> Notify user
                   move "Reached end of input file ( ੭ˊᵕˋ)੭" to output-buffer
                   perform outputLine
           end-read
       else
           string
               "Error reading input file: " delimited by size
               input-file-status            delimited by size
               " (｡•́︿•̀｡)"                   delimited by size
               into output-buffer
           end-string
           perform outputLine
       end-if
       exit.


*> Paragraph: outputLine
*> Purpose:   Prints string in buffer to console and saves to output log
*> Input:     output-buffer - string you want to be output
*> Output:    None
outputLine.
       open extend output-file.

       if output-file-status = "35"
           *> Log file does not exist yet, so create it
           open output output-file
       end-if

       *> Ensure file opened properly
       if output-file-status = "00"
           *> If we want to ensure that the console and output file are the same,
           *> console output can only happen if output file opens
           display output-buffer

           move output-buffer to output-line
           write output-line

           close output-file
       else
           display "Error opening output file: " output-file-status " (っ- ‸ - ς)"
       end-if

       *> Some characters get 'stuck', manually clearing fixes it though
       move spaces to output-buffer.
       exit.
