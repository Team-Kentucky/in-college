IDENTIFICATION DIVISION.
program-id. INCOLLEGE.
*>###################################################################
*> InCollege Project - Commit 1: Basic Menu System and Data Structures
*> This commit establishes the main menu structure and defines
*> data structures for user accounts
*>###################################################################
ENVIRONMENT DIVISION.
input-output section.
file-control.
       *> Input file for automated testing
       select input-file assign to 'InCollege-Input.txt'
           organization is line sequential
           file status is input-file-status.
       *> Output file to preserve program output
       select output-file assign to 'InCollege-Output.txt'
           organization is line sequential
           file status is output-file-status.
       *> Accounts file for persistence
       select accounts-file assign to 'accounts.dat'
           organization is line sequential
           file status is accounts-file-status.

*>###################################################################
DATA DIVISION.
file section.
*> Input file record
fd input-file.
01 input-buffer pic x(100).

*> Output file record  
fd output-file.
01 output-file-record pic x(100).

*> Accounts file record
fd accounts-file.
01 account-record.
   05 username pic x(30).
   05 password pic x(12).

working-storage section.
*> File status variables
01 input-file-status pic xx.
01 output-file-status pic xx.
01 accounts-file-status pic xx.
01 output-buffer pic x(100).

*> Menu choice variables
01 menu-choice pic x(1).
01 valid-choice pic x(1) value 'N'.

*> Account management variables
01 account-count pic 9(1) value 0.
01 max-accounts pic 9(1) value 5.

*> User input variables
01 input-username pic x(30).
01 input-password pic x(12).

*> Control flags
01 program-running pic x(1) value 'Y'.
01 logged-in pic x(1) value 'N'.
01 current-user pic x(30).

*> Password validation working variables
01 pwd-raw              pic x(50).
01 pwd-length           pic 9(2) value 0.
01 idx                  pic 9(2) value 1.
01 pwd-ch               pic x(1).
01 has-capital          pic x(1) value 'N'.
01 has-digit            pic x(1) value 'N'.
01 has-special          pic x(1) value 'N'.

*> Constants for menu display
01 welcome-msg pic x(22) value "Welcome to InCollege!".
01 login-option pic x(7) value "Log In".
01 create-option pic x(18) value "Create New Account".
01 choice-prompt pic x(18) value "Enter your choice:".

*> ASCII-art menu elements and end marker
01 menu-border-top     pic x(60) value "==============================================".
01 menu-title-line     pic x(60) value "||             Welcome to InCollege          ||".
01 menu-border-mid     pic x(60) value "||------------------------------------------||".
01 menu-border-bottom  pic x(60) value "==============================================".
01 end-marker          pic x(40) value "--- END_OF_PROGRAM_EXECUTION ---".

local-storage section.

*>###################################################################
PROCEDURE DIVISION.
main.
       *> Initialize program
       perform initialize-program
       
       *> Main program loop
       perform until program-running = 'N'
           perform display-main-menu
           perform get-menu-choice
           perform process-main-menu-choice
       end-perform
       
       *> Clean up and exit
       perform cleanup-program
       stop run.

*>*******************************************************************
*> Initialize program - open files and display welcome
*>*******************************************************************
initialize-program.
       *> Display welcome message
       move welcome-msg to output-buffer
       perform output-line
       
       *> Open input file for reading user choices
       open input input-file
       if input-file-status not = "00"
           move "Error opening input file" to output-buffer
           perform output-line
       end-if

       *> Initialize or load existing accounts count
       move 0 to account-count
       open input accounts-file
       evaluate accounts-file-status
           when "35"
               *> File missing: no accounts yet
               continue
           when "00"
               perform until accounts-file-status not = "00"
                   read accounts-file
                       at end
                           exit perform
                       not at end
                           add 1 to account-count
                   end-read
               end-perform
               close accounts-file
           when other
               move "Error opening accounts file" to output-buffer
               perform output-line
       end-evaluate
       exit.

*>*******************************************************************
*> Display the main menu options
*>*******************************************************************
display-main-menu.
       move menu-border-top to output-buffer
       perform output-line
       move menu-title-line to output-buffer
       perform output-line
       move menu-border-mid to output-buffer
       perform output-line
       move login-option to output-buffer
       perform output-line
       move create-option to output-buffer
       perform output-line
       move choice-prompt to output-buffer
       perform output-line
       exit.

*>*******************************************************************
*> Get menu choice from input file
*>*******************************************************************
get-menu-choice.
       perform read-input-line
       move input-buffer(1:1) to menu-choice
       exit.

*>*******************************************************************
*> Process the main menu choice
*>*******************************************************************
process-main-menu-choice.
       evaluate menu-choice
           when '1'
               perform login-process
           when '2'  
               perform create-account-process
           when other
               move "Invalid choice. Please try again." to output-buffer
               perform output-line
       end-evaluate
       exit.

*>*******************************************************************
*> Login process placeholder - will be implemented in commit 3
*>*******************************************************************
login-process.
       move "Login functionality - Under Construction" to output-buffer
       perform output-line
       exit.

*>*******************************************************************
*> Create account process placeholder - will be implemented in commit 2  
*>*******************************************************************
create-account-process.
       *> Check account limit first
       if account-count >= max-accounts
           move "All permitted accounts have been created, please come back later" to output-buffer
           perform output-line
           exit paragraph
       end-if

       *> Prompt for username
       move "Please enter your username:" to output-buffer
       perform output-line
       perform read-input-line
       move function trim(input-buffer trailing) to input-username

       *> Check if username already exists (scan file)
       move 'N' to valid-choice
       open input accounts-file
       if accounts-file-status = "00"
           perform until accounts-file-status not = "00"
               read accounts-file
                   at end
                       exit perform
                   not at end
                       if function trim(username trailing) = function trim(input-username trailing)
                           move 'Y' to valid-choice
                           exit perform
                       end-if
               end-read
           end-perform
           close accounts-file
       end-if

       if valid-choice = 'Y'
           move "Username already exists. Please try again." to output-buffer
           perform output-line
           exit paragraph
       end-if

       *> Prompt for password
       move "Please enter your password:" to output-buffer
       perform output-line
       perform read-input-line

       *> Validate password: 8-12 chars, at least one capital, one digit, one special
       perform validate-password
       if valid-choice not = 'Y'
           move "Invalid password, please try again" to output-buffer
           perform output-line
           exit paragraph
       end-if

        *> Save account to file
       move function trim(input-username trailing) to username
       move function trim(input-password trailing) to password
       open extend accounts-file
       if accounts-file-status = "35"
           open output accounts-file
       end-if
       if accounts-file-status = "00"
           write account-record
           close accounts-file
           add 1 to account-count
           move "Account created successfully." to output-buffer
           perform output-line
       else
           move "Error writing to accounts file" to output-buffer
           perform output-line
       end-if
       exit.

*>*******************************************************************
*> Password validation routine
*>*******************************************************************
validate-password.
       *> Capture and trim raw password from input-buffer
       move function trim(input-buffer trailing) to pwd-raw
       compute pwd-length = function length(function trim(pwd-raw trailing))

       *> Initialize result to invalid
       move 'N' to valid-choice

       *> Length check 8..12
       if pwd-length < 8 or pwd-length > 12
           exit paragraph
       end-if

       *> Scan characters
       perform varying idx from 1 by 1 until idx > pwd-length
           move pwd-raw(idx:1) to pwd-ch
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
           move 'Y' to valid-choice
           move pwd-raw(1:12) to input-password
       end-if
       exit.

*>*******************************************************************
*> Clean up resources before exit
*>*******************************************************************
cleanup-program.
       *> Close input file if open
       if input-file-status = "00"
           close input-file
       end-if
       *> Print end-of-program marker
       move end-marker to output-buffer
       perform output-line
       exit.

*>*******************************************************************
*> Read next line from input file
*>*******************************************************************
read-input-line.
       if input-file-status = "00"
           read input-file
               at end
                   move spaces to input-buffer
                   move 'N' to program-running
           end-read
       else
           move spaces to input-buffer
           move 'N' to program-running
       end-if
       exit.

*>*******************************************************************
*> Output line to both console and file
*>*******************************************************************
output-line.
       open extend output-file
       if output-file-status = "35"
           *> File doesn't exist, create it
           open output output-file
       end-if
       
       if output-file-status = "00"
           display output-buffer
           move output-buffer to output-file-record
           write output-file-record
           close output-file
       else
           display "Error opening output file: " output-file-status
       end-if
       
       move spaces to output-buffer
       exit.