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
01 end-marker          pic x(30) value "--- END_OF_PROGRAM_EXECUTION ---".

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
       move "Create account functionality - Under Construction" to output-buffer
       perform output-line
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