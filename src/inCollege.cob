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
fd input-file.
01 input-line pic x(100). *> Need to decide on resonable size for lines

fd output-file.
01 output-line pic x(100).

working-storage section.
01 input-buffer pic x(100).
01 input-file-status pic xx.

01 output-buffer pic x(100).
01 output-file-status pic xx.

local-storage section.


*>###################################################################
PROCEDURE DIVISION.
       move "test output line" to output-buffer.
       perform outputLine



       stop run.

*> Gets the user's next line of input
*> Needs to ensure what we are reading from is valid
*> If file is invalid, set user input string to be blank?
readInputLine.
       *> Open and close each time?? Could then check if it is 00
       *> Will not have a saved place then. Should probably require user to open and close file
       *> Will need to check for all of the other flags

       if input-file-status = "00"
           read input-file
               at end
                   display " "
               not at end
                   move input-line to input-buffer *>Maybe???
           end-read
       else
           display "Error opening input file: " output-file-status
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
           display output-buffer

           move output-buffer to output-line
           write output-line

           close output-file
       else
           display "Error opening output file: " output-file-status
       end-if
       exit.
