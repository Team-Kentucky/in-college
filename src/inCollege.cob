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

file section.
*>-----readInputLine variables-----
*> Input file record
fd input-file.
01 input-buffer pic x(100).

*>-----outputLine variables-----
*> Output file record
fd output-file.
01 output-line pic x(150).

*> Accounts file record
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

*> Control flags
01 logged-in pic x(1) value 'N'.
01 current-user pic x(30).

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
01 buffer-acct-password pic x(100).
01 num-accounts pic 99.

*> Password validation working variables
01 password-validity pic x.
       88 valid-password value 'Y'.
       88 invalid-password value 'N'.
01 pwd-raw              pic x(50).
01 pwd-length           pic 9(2) value 0.
01 idx                  pic 9(2) value 1.
01 pwd-ch               pic x(1).
01 has-capital          pic x(1) value 'N'.
01 has-digit            pic x(1) value 'N'.
01 has-special          pic x(1) value 'N'.

*> User input variables
01 input-username pic x(30).
01 input-password pic x(12).
01 valid-choice pic x(1) value 'N'.
01 menu-choice pic x(1).
01 welcome-page-selection pic x(100).

*> Menu constants
01 incorrect-login-msg constant as "Incorrect username/password, please try again".
01 success-login-msg   constant as "You have successfully logged in".
01 welcome-user-prefix constant as "Welcome, ".
01 welcome-user-line   pic x(60).
01 choice-prompt constant as "Enter your choice:".
01 post-login-1 constant as "[1] Search for a job".
01 post-login-2 constant as "[2] Find someone you know".
01 post-login-3 constant as "[3] Learn a new skill".
01 logout constant as "[q] Logout".
01 under-construction  constant as "is under construction.".
01 uc-job-prefix       constant as "Job search/internship ".
01 uc-find-prefix      constant as "Find someone you know ".
01 skills-title        constant as "Learn a New Skill:".
01 skill1              constant as "[1] Skill 1".
01 skill2              constant as "[2] Skill 2".
01 skill3              constant as "[3] Skill 3".
01 skill4              constant as "[4] Skill 4".
01 skill5              constant as "[5] Skill 5".
01 go-back             constant as "[q] Go Back".
01 END-MARKER          constant as "--- END_OF_PROGRAM_EXECUTION ---".

*> Profile fields
01 profile-first-name      pic x(30).
01 profile-last-name       pic x(30).
01 profile-university      pic x(50).
01 profile-major           pic x(30).
01 profile-grad-year       pic x(4).
01 grad-year-length   pic 99.
01 profile-about-me        pic x(200).

01 exp-idx                 pic 9 value 1.
01 profile-exp-title       occurs 3 pic x(30).
01 profile-exp-company     occurs 3 pic x(30).
01 profile-exp-dates       occurs 3 pic x(20).
01 profile-exp-desc        occurs 3 pic x(100).

01 edu-idx                 pic 9 value 1.
01 profile-edu-degree      occurs 3 pic x(30).
01 profile-edu-university  occurs 3 pic x(50).
01 profile-edu-years       occurs 3 pic x(20).

local-storage section.


*>###################################################################
PROCEDURE DIVISION.
main.
       perform initialize-program

       perform displayLogo.
       perform welcomePage.

       *> Clean up and exit
       perform cleanup-program
       stop run.


*>*******************************************************************
*> Initialize program - open files and display welcome
*>*******************************************************************
initialize-program.
       *> Open input file for reading user choices
       open input input-file
       if input-file-status not = "00"
           move "Error opening input file" to output-buffer
           perform outputLine
       end-if
       exit.


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
                   perform login-process
               when welcome-page-selection = '1'
                   perform accountCreation
               when other
                   move "Invalid input" to output-buffer
                   perform outputLine
           end-evaluate
       end-perform.
       exit.


*>*******************************************************************
*> Login process placeholder - will be implemented in commit 3
*>*******************************************************************
login-process.
       *> Unlimited attempts until successful login
       perform until logged-in = 'Y' or not valid-read
           move "Please enter your username:" to output-buffer
           perform outputLine
           perform readInputLine
           move function trim(input-buffer trailing) to input-username

           move "Please enter your password:" to output-buffer
           perform outputLine
           perform readInputLine
           move function trim(input-buffer trailing) to input-password


           move input-username to buffer-acct-username
           perform findAcct
           if acct-found and function trim(buffer-acct-password trailing) = function trim(input-password trailing)
               move 'Y' to valid-choice
           end-if

           if valid-choice = 'Y'
               move success-login-msg to output-buffer
               perform outputLine
               move spaces to welcome-user-line
               string welcome-user-prefix delimited by size
                      input-username delimited by space
                      into welcome-user-line
               end-string
               move welcome-user-line to output-buffer
               perform outputLine
               move 'Y' to logged-in
               move input-username to current-user
               perform post-login-menu
               move 'N' to logged-in
               move 'N' to valid-choice
               exit perform
           else
               move incorrect-login-msg to output-buffer
               perform outputLine
           end-if
       end-perform
       exit.


*>*******************************************************************
*> Post-login menu and navigation
*>*******************************************************************
post-login-menu.
       perform until logged-in = 'N' or not valid-read
           move post-login-1 to output-buffer
           perform outputLine
           move post-login-2 to output-buffer
           perform outputLine
           move post-login-3 to output-buffer
           perform outputLine
           move logout to output-buffer
           perform outputLine
           move choice-prompt to output-buffer
           perform outputLine

           perform readInputLine
           move input-buffer(1:1) to menu-choice

           evaluate true
               when menu-choice = '1'
                   perform profileCreation
               when menu-choice = '2'
                   *> Find someone under construction
                   move spaces to output-buffer
                   string uc-find-prefix delimited by size
                          under-construction delimited by size
                          into output-buffer
                   end-string
                   perform outputLine
               when menu-choice = '3'
                   perform skills-menu
               when menu-choice = 'q' or not valid-read
                   exit perform
               when other
                   move "Invalid choice. Please try again." to output-buffer
                   perform outputLine
           end-evaluate
       end-perform
       exit.


*>*******************************************************************
*> Skills list with option to go back
*>*******************************************************************
skills-menu.
       perform until not valid-read
           move skills-title to output-buffer
           perform outputLine
           move skill1 to output-buffer
           perform outputLine
           move skill2 to output-buffer
           perform outputLine
           move skill3 to output-buffer
           perform outputLine
           move skill4 to output-buffer
           perform outputLine
           move skill5 to output-buffer
           perform outputLine
           move go-back to output-buffer
           perform outputLine
           move choice-prompt to output-buffer
           perform outputLine

           perform readInputLine
           move input-buffer(1:1) to menu-choice

           evaluate true
               when menu-choice = '1'
                   move "This skill is under construction." to output-buffer
                   perform outputLine
               when menu-choice = '2'
                   move "This skill is under construction." to output-buffer
                   perform outputLine
               when menu-choice = '3'
                   move "This skill is under construction." to output-buffer
                   perform outputLine
               when menu-choice = '4'
                   move "This skill is under construction." to output-buffer
                   perform outputLine
               when menu-choice = '5'
                   move "This skill is under construction." to output-buffer
                   perform outputLine
               when menu-choice = 'q' or not valid-read
                   exit perform
               when other
                   move "Invalid input" to output-buffer
                   perform outputLine
           end-evaluate
       end-perform
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

           perform with test after until acct-not-found or not valid-read
                move "Username: " to input-prompt
                perform readInputLine
                move input-buffer to buffer-acct-username

                perform findAcct
                if acct-found
                   move "Username has already been taken" to output-buffer
                   perform outputLine
                end-if
           end-perform

           perform with test after until valid-password or not valid-read
               move "Password: " to input-prompt
               perform readInputLine
               move input-buffer to buffer-acct-password

               perform validate-password
               if not valid-password
                   move "Password must be between 8-12 characters, contain 1 capital letter, 1 digit, and 1 special character" to output-buffer
                   perform outputLine
               end-if
           end-perform

           if valid-password and valid-read
               perform addAcct
               *> If all works, run this:
               perform outputLine
               move "Account has successfully been created" to output-buffer
               perform outputLine
           end-if
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

       move zero to num-accounts

       if acct-database-status = "00"
           move buffer-acct-username to acct-username
           perform until not database-good-read
               read acct-database
                   not at end
                       add 1 to num-accounts
               end-read
           end-perform
           *> Since it doesn't count the one we started on
           add 1 to num-accounts
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


*>*******************************************************************
*> Password validation routine
*>*******************************************************************
validate-password.
       *> Capture and trim raw password from input-buffer
       move function trim(buffer-acct-password trailing) to pwd-raw
       compute pwd-length = function length(function trim(pwd-raw trailing))

       *> Initialize result to invalid
       move 'N' to password-validity
       move 'N' to has-capital
       move 'N' to has-digit
       move 'N' to has-special

       *> Length check 8..12
       if pwd-length < 8 or pwd-length > 12
           exit paragraph
       end-if

       *> NOT WORKING
       *> Scan characters
       perform varying idx from 1 by 1 until idx > pwd-length
           move pwd-raw(idx:idx) to pwd-ch
           evaluate true
               when pwd-ch >= 'A' and pwd-ch <= 'Z'
                   move 'Y' to has-capital
               when pwd-ch >= '0' and pwd-ch <= '9'
                   move 'Y' to has-digit
               when (pwd-ch >= 'a' and pwd-ch <= 'z')
                   continue
               when other
                   move 'Y' to has-special
           end-evaluate
       end-perform

       if has-capital = 'Y' and has-digit = 'Y' and has-special = 'Y'
           move 'Y' to password-validity
           move pwd-raw(1:12) to input-password
       end-if
       exit.

profileCreation.
       move "Create/Edit My Profile" to output-buffer
       perform outputLine
       perform displayDashedLine

       *> First Name (Required)
       perform with test after until profile-first-name not = spaces or not valid-read
           move "Enter First Name (Required):" to input-prompt
           perform readInputLine
           move function trim(input-buffer trailing) to profile-first-name
       end-perform

       *> Last Name (Required)
       perform with test after until profile-last-name not = spaces or not valid-read
           move "Enter Last Name (Required):" to input-prompt
           perform readInputLine
           move function trim(input-buffer trailing) to profile-last-name
       end-perform

       *> University/College Attended (Required)
       perform with test after until profile-university not = spaces or not valid-read
           move "Enter University/College Attended (Required):" to input-prompt
           perform readInputLine
           move function trim(input-buffer trailing) to profile-university
       end-perform

       *> Major (Required)
       perform with test after until profile-major not = spaces or not valid-read
           move "Enter Major (Required):" to input-prompt
           perform readInputLine
           move function trim(input-buffer trailing) to profile-major
       end-perform

       *> Graduation Year (Required, must be 4 digits)
          perform until valid-read
              and function length(function trim(input-buffer trailing)) = 4
              and function trim(input-buffer trailing) numeric

               move "Enter Graduation Year (Required, e.g., 2025):"
                    to input-prompt
               perform readInputLine

           if not valid-read
              exit paragraph
           end-if

           if function length(function trim(input-buffer trailing)) not = 4
              or function trim(input-buffer trailing) not numeric
              move "Graduation Year must be a valid 4-digit number."
                   to output-buffer
              perform outputLine
           end-if

       end-perform

       *> Safe now: input is exactly 4 digits
       move function trim(input-buffer trailing) to profile-grad-year

      *> About me (Optional)

      *> Experience section (Optional, up to 3 entries). Inputs: 'DONE' to skip, for other inputs: Experience #1 - Title:
            *> Experience #1 - Company/Organization:
            *> Experience #1 - Dates (e.g., Summer 2024):
            *> Experience #1 - Description (optional, max 100 chars, blank to skip):

      *> Same for Education


       move "Profile saved successfully!" to output-buffer
       perform outputLine
       exit paragraph.


*>*******************************************************************
*> Clean up resources before exit
*>*******************************************************************
cleanup-program.
       *> Close input file if open
       close input-file
       *> Print end-of-program marker
       move end-marker to output-buffer
       perform outputLine
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
