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
01 output-line pic x(150).

fd acct-database.
copy "account.cpy".

working-storage section.
*>-----readInputLine variables-----
01 input-prompt pic x(100).
01 input-file-status pic xx.
       88 valid-read value "00".

*>-----outputLine variables-----
01 output-buffer pic x(150).
01 output-file-status pic xx.

*>-----logIn variables-----
01 welcome-page-selection pic x(100).

*>-----account database variables-----
01 acct-database-status pic xx.
       88 user-already-exists value "22".
       88 database-does-not-exist value "35".
       88 database-good-read value "00".

01 acct-status pic x.
       88 acct-found value "1".
       88 acct-not-found value "2".
       88 duplicate-user value "3".
       88 new-user value "4".

01 buffer-acct-username pic x(100).
01 buffer-acct-password pic x(12).
01 num-accounts pic 99.


local-storage section.


*>###################################################################
PROCEDURE DIVISION.
main.
       open input input-file.

       perform displayLogo.
       perform welcomePage.

       close input-file.
       stop run.


*> Paragraph: welcomePage
*> Purpose:   First menu of the program. Allows the user to sign in or create an account
*> Input:     None
*> Output:    None
welcomePage.
       perform with test after until (welcome-page-selection = 'q' or welcome-page-selection = 'Q' or not valid-read)
           perform outputLine
           perform displayDashedLine
           move "Welcome to inCollege! Select an option" to output-buffer
           perform outputLine
           perform displayDashedLine

           move " [0] Sign in" to output-buffer
           perform outputLine
           move " [1] Create an account" to output-buffer
           perform outputLine
           move " [q] Quit" to output-buffer
           perform outputLine
           move "> " to input-prompt
           perform readInputLine

           move input-buffer to welcome-page-selection
           evaluate true
               when (welcome-page-selection = 'q' or welcome-page-selection = 'Q' or not valid-read)
                   continue
               when welcome-page-selection = '0'
                   perform signIn
               when welcome-page-selection = '1'
                   perform accountCreation
               when other
                   move "Invalid input" to output-buffer
                   perform outputLine
           end-evaluate
       end-perform.
       exit.


signIn.
       *>stub
       exit.


accountCreation.
       *> Verify that we aren't at max accounts
       perform findNumAccounts.
       if num-accounts < 6
           perform outputLine
           perform displayDashedLine
           move "Please enter all required information" to output-buffer
           perform outputLine
           perform displayDashedLine
           perform outputLine

           perform with test after until acct-not-found
                move "Username: " to input-prompt
                perform readInputLine
                move input-buffer to buffer-acct-username

                perform findAcct
                if acct-found
                   move "Username has already been taken" to output-buffer
                   perform outputLine
                end-if
           end-perform

           *> perform with test after until valid-password
               move "Password: " to input-prompt
               perform readInputLine
               move input-buffer to buffer-acct-password

               *> perform password-validation
               *>if not valid-password
                   *>move "Password must be between 8-12 characters, contain 1 capital letter, 1 digit, and 1 special character"
                   *>perform outputLine
               *>end-if
           *>end-perform

           perform addAcct
           *> If all works, run this:
           perform outputLine
           move "Account has successfully been created" to output-buffer
           perform outputLine
       else
               move "All permitted accounts have been created, please come back later" to output-buffer
               perform outputLine
       end-if
       exit.


*> Paragraph: acctDatabaseSize
*> Purpose:   Finds the total number of accounts in the database
*> Input:     None
*> Output:    num-accounts
findNumAccounts.
       open input acct-database.

       if acct-database-status = "00"
           move buffer-acct-username to acct-username
           perform until not database-good-read
               read acct-database
                   not at end
                       add 1 to num-accounts
               end-read
           end-perform
       else if database-does-not-exist
           move 0 to num-accounts
       else
           string
               "Error opening account database: " delimited by size
               acct-database-status               delimited by size
               into output-buffer
           end-string
           perform outputLine
       end-if
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

           if user-already-exists move "3" to acct-status
           else move "4" to acct-status
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
                   move '2' to acct-status
               not invalid key
                   move '1' to acct-status
                   move acct-password to buffer-acct-password
           end-read
       else if database-does-not-exist
           move '2' to acct-status
       else
           string
               "Error opening account database: " delimited by size
               acct-database-status               delimited by size
               into output-buffer
           end-string
           perform outputLine
       end-if
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
               not at end
                   *>Simulating the user entering the input
                   string
                       function trim(input-prompt, trailing) delimited by size
                       " " delimited by size                                   *> Will always have an extra space (even if input has no prompt)
                       function trim(input-buffer, trailing) delimited by size
                       into output-buffer
                   end-string
                   perform outputLine
               at end
                   move input-prompt to output-buffer
                   perform outputLine

                   *> Input-buffer is stale now, so set to spaces
                   move spaces to input-buffer
                   *> Notify user
                   move "Reached end of input file ( ੭ˊᵕˋ)੭" to output-buffer
                   *> We should eithter
                       *> Make this trigger a use when to end the program?
                       *> Have the user be responsible for gracefully closing
                       *> Switch to manual input from accept

                   perform outputLine
           end-read
       else
           move input-prompt to output-buffer
           perform outputLine

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
*> Input:     output-buffer - string you want to be output. Will be cleared
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
           move output-buffer to output-line
           display function trim(output-line, trailing) *> Will make logged output have extra spaces compared to command line. Do we care?
           write output-line

           close output-file
       else
           display "Error opening output file: " output-file-status " (っ- ‸ - ς)"
       end-if

       *> Some characters get 'stuck', manually clearing fixes it though
       move spaces to output-buffer.
       exit.


*> Paragraph: displayLogo
*> Purpose:   Prints the beautiful ascii logo for us
*> Input:     None
*> Output:    None
*> ASCII art was created with https://patorjk.com/software/taag/
displayLogo.
       perform displayASCIILine.
       perform outputLine.
       move "           /##            /######            /## /##                                        " to output-buffer.
       perform outputLine.
       move "          |__/           /##__  ##          | ##| ##                                        " to output-buffer.
       perform outputLine.
       move "           /## /####### | ##  \__/  /###### | ##| ##  /######   /######   /######           " to output-buffer.
       perform outputLine.
       move "          | ##| ##__  ##| ##       /##__  ##| ##| ## /##__  ## /##__  ## /##__  ##          " to output-buffer.
       perform outputLine.
       move "          | ##| ##  \ ##| ##      | ##  \ ##| ##| ##| ########| ##  \ ##| ########          " to output-buffer.
       perform outputLine.
       move "          | ##| ##  | ##| ##    ##| ##  | ##| ##| ##| ##_____/| ##  | ##| ##_____/          " to output-buffer.
       perform outputLine.
       move "          | ##| ##  | ##|  ######/|  ######/| ##| ##|  #######|  #######|  #######          " to output-buffer.
       perform outputLine.
       move "          |__/|__/  |__/ \______/  \______/ |__/|__/ \_______/ \____  ## \_______/          " to output-buffer.
       perform outputLine.
       move "                                                               /##  \ ##                    " to output-buffer.
       perform outputLine.
       move "                                                              |  ######/                    " to output-buffer.
       perform outputLine.
       move "                                                               \______/            	     " to output-buffer.
       perform outputLine.
       move "                                                                             ฅ^•ﻌ•^ฅ        " to output-buffer.
       perform outputLine.
       perform displayASCIILine.
       move "                                                                    Created by Team Kentucky" to output-buffer.
       perform outputLine.
       move "                            The world's best job site for students                          " to output-buffer.
       perform outputLine.
       perform outputLine.
       perform outputLine.
       exit.


displayASCIILine.
       move "############################################################################################" to output-buffer.
       perform outputLine.
       exit.


displayDashedLine.
       move "————————————————————————————————————————————————————————————————————————————————————————————" to output-buffer.
       perform outputLine.
       exit.
