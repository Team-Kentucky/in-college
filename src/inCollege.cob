IDENTIFICATION DIVISION.
program-id. INCOLLEGE.


*>###################################################################
ENVIRONMENT DIVISION.

input-output section.
file-control.
       select input-file assign to 'input.txt'
           organization is line sequential
           file status is input-file-status.


*>###################################################################
DATA DIVISION.

file section.
fd input-file.
01 input-line pic x(100). *> Need to decide on resonable size for lines

working-storage section.
01 input-file-status pic xx.
01 eof pic x value 'N'.

local-storage section.


*>###################################################################
PROCEDURE DIVISION.

       stop run.
