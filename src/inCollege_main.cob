IDENTIFICATION DIVISION.
program-id. INCOLLEGE.


*>###################################################################

ENVIRONMENT DIVISION.

input-output section.
file-control.
       *> Input file for automated testing
       select input-file assign to 'input.txt'
           organization is line sequential
           file status is input-file-status.
       *> Output file to preserve program output
       select output-file assign to 'output.txt'
           organization is line sequential
           file status is output-file-status.
       *> Accounts file for persistence
       select acct-database assign to 'acct-database.dat'
           organization is indexed
           access mode is dynamic
           record key is acct-username
           file status is acct-database-status.

*>###################################################################

DATA DIVISION.
COPY 'INCOLLEGE-FILE.cpy'.
COPY 'INCOLLEGE-WS.cpy'.
COPY 'INCOLLEGE-LS.cpy'.

PROCEDURE DIVISION.
COPY 'INCOLLEGE-PROC.cpy'.
