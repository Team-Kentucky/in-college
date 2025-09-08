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

       select acct-database assign to 'acct-database.dat'
           organization is indexed
           access mode is dynamic
           record key is acct-username
           file status is acct-database-status.

*>###################################################################
DATA DIVISION.

file section.
*>-----readInputLine variables-----
fd input-file.
01 input-buffer pic x(100). *> Need to decide on reasonable size for lines

*>-----outputLine variables-----
fd output-file.
01 output-line pic x(100).

fd acct-database.
copy "account.cpy".

working-storage section.
*>-----readInputLine variables-----
01 input-file-status pic xx.

*>-----outputLine variables-----
01 output-buffer pic x(100).
01 output-file-status pic xx.

*>-----account database variables-----
01 acct-database-status pic xx.
       88 user-already-exists value "22".
       88 database-does-not-exist value "35".

01 acct-exists pic x.
       88 acct-found value "Y".
       88 acct-not-found value "N".

01 new-user-added pic x.
       88 duplicate-user value "N".
       88 unique-user value "Y".

*>copy "account.cpy"
*>   replacing ==acct== by ==local-acct==.
01 buffer-acct-username pic x(100).
01 buffer-acct-password pic x(12).
01 acct-count pic 99.


local-storage section.


*>###################################################################
PROCEDURE DIVISION.
main.
       move "matthe2983" to buffer-acct-username.
       move "18979" to buffer-acct-password.
       perform addAcct.

       move "matthe2983" to buffer-acct-username.
       move "189798" to buffer-acct-password.
       perform addAcct.

       if duplicate-user
           display "account already exists".

       move "testacct" to buffer-acct-username.
       move "1234567" to buffer-acct-password.
       perform addAcct.

       move "testa" to buffer-acct-username.
       move "123456789" to buffer-acct-password.
       perform addAcct.

       move "matthe2983" to buffer-acct-username.
       perform findAcct.
       display acct-password.

       move "notIndata" to buffer-acct-username.
       perform findAcct.

       perform acctDatabaseSize.
       display acct-count.


       stop run.


acctDatabaseSize.
       open input acct-database.

       if acct-database-status = "00"
           move buffer-acct-username to acct-username
           perform until acct-database-status not = "00"
               read acct-database
                   not at end
                       add 1 to acct-count
               end-read
           end-perform
       else
           string
               "Error opening account database: " delimited by size
               acct-database-status               delimited by size
               into output-buffer
           end-string
           perform outputLine
       end-if
       close acct-database.
       exit.

*> Paragraph: readInputLine
*> Purpose:   Adds an account to the account database
*> Input:     buffer-acct-username
*>            buffer-acct-password
*> Output:    None
*> User should verify if the new record was duplicate
addAcct.
       open i-o acct-database.

       if database-does-not-exist
           *> Log file does not exist yet, so create it
           open output acct-database
       end-if


       if acct-database-status = "00"
           move buffer-acct-username to acct-username
           move buffer-acct-password to acct-password
           write acct-record

           if user-already-exists move "N" to new-user-added
           else move "Y" to new-user-added
       else
           string
               "Error opening account database: " delimited by size
               acct-database-status               delimited by size
               into output-buffer
           end-string
           perform outputLine
       end-if
       close acct-database.
       exit.

*> Paragraph: readInputLine
*> Purpose:   Finds an account in the account database
*> Input:     buffer-acct-username
*> Output:    None
findAcct.
       open input acct-database.

       if acct-database-status = "00"
           move buffer-acct-username to acct-username
           read acct-database
               key is acct-username
               invalid key
                   move 'N' to acct-exists
               not invalid key
                   move 'Y' to acct-exists
           end-read
       else
           string
               "Error opening account database: " delimited by size
               acct-database-status               delimited by size
               into output-buffer
           end-string
           perform outputLine
       end-if
       close acct-database.

       exit.


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
