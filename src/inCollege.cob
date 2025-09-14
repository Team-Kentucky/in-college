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

*> Profile management variables
01 profile-input-buffer pic x(200).
01 profile-validation-flag pic x(1).
   88 profile-valid value 'Y'.
   88 profile-invalid value 'N'.
01 profile-year-numeric pic 9(4).
01 profile-year-string pic x(10).
01 profile-entry-count pic 9(1).
01 profile-loop-index pic 9(1).

*> Profile data working storage
01 ws-profile-first-name pic x(50).
01 ws-profile-last-name pic x(50).
01 ws-profile-university pic x(100).
01 ws-profile-major pic x(100).
01 ws-profile-graduation-year pic 9(4).
01 ws-profile-about-me pic x(200).
01 ws-profile-experience.
   05 ws-experience-entry occurs 3 times.
       10 ws-exp-title pic x(100).
       10 ws-exp-company pic x(100).
       10 ws-exp-dates pic x(50).
       10 ws-exp-description pic x(100).
01 ws-profile-education.
   05 ws-education-entry occurs 3 times.
       10 ws-edu-degree pic x(100).
       10 ws-edu-university pic x(100).
       10 ws-edu-years pic x(50).
01 ws-profile-initialized pic x(1).
   88 ws-profile-exists value 'Y'.
   88 ws-profile-empty value 'N'.

*> Menu constants
01 incorrect-login-msg constant as "Incorrect username/password, please try again".
01 success-login-msg   constant as "You have successfully logged in".
01 welcome-user-prefix constant as "Welcome, ".
01 welcome-user-line   pic x(60).
01 choice-prompt constant as "Enter your choice:".
01 post-login-1 constant as "[1] Create/Edit My Profile".
01 post-login-2 constant as "[2] View My Profile".
01 post-login-3 constant as "[3] Search for User".
01 post-login-4 constant as "[4] Learn a new skill".
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
01 end-marker          constant as "--- END_OF_PROGRAM_EXECUTION ---".

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
           move post-login-4 to output-buffer
           perform outputLine
           move logout to output-buffer
           perform outputLine
           move choice-prompt to output-buffer
           perform outputLine

           perform readInputLine
           move input-buffer(1:1) to menu-choice

           evaluate true
               when menu-choice = '1'
                   perform profile-create-edit-menu
               when menu-choice = '2'
                   perform profile-view-menu
               when menu-choice = '3'
                   *> Search for User under construction
                   move "Search for User is under construction." to output-buffer
                   perform outputLine
               when menu-choice = '4'
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
           *> Initialize profile data for new account
           perform initialize-profile
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


*>*******************************************************************
*> Profile data management functions
*>*******************************************************************

*> Paragraph: initialize-profile
*> Purpose:   Initialize profile data for a new user
*> Input:     None
*> Output:    None
initialize-profile.
    move spaces to ws-profile-first-name
    move spaces to ws-profile-last-name
    move spaces to ws-profile-university
    move spaces to ws-profile-major
    move 0 to ws-profile-graduation-year
    move spaces to ws-profile-about-me
    
    *> Initialize experience entries
    perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
        move spaces to ws-exp-title(profile-loop-index)
        move spaces to ws-exp-company(profile-loop-index)
        move spaces to ws-exp-dates(profile-loop-index)
        move spaces to ws-exp-description(profile-loop-index)
    end-perform
    
    *> Initialize education entries
    perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
        move spaces to ws-edu-degree(profile-loop-index)
        move spaces to ws-edu-university(profile-loop-index)
        move spaces to ws-edu-years(profile-loop-index)
    end-perform
    
    move 'N' to ws-profile-initialized
    exit.

*> Paragraph: validate-graduation-year
*> Purpose:   Validate graduation year input
*> Input:     profile-year-string
*> Output:    profile-validation-flag
validate-graduation-year.
    move 'N' to profile-validation-flag
    
    *> Check if year is exactly 4 digits
    if function length(function trim(profile-year-string trailing)) = 4
        *> Try to convert to numeric - if it fails, it's invalid
        move function numval(profile-year-string) to profile-year-numeric
        if profile-year-numeric >= 1900 and profile-year-numeric <= 2100
            move 'Y' to profile-validation-flag
        end-if
    end-if
    exit.

*> Paragraph: save-profile-data
*> Purpose:   Save profile data to the account database
*> Input:     current-user, profile data
*> Output:    None
save-profile-data.
    open i-o acct-database.
    
    if acct-database-status = "00"
        move current-user to acct-username
        read acct-database
            key is acct-username
            invalid key
                move "Error: User not found for profile save" to output-buffer
                perform outputLine
            not invalid key
                *> Copy profile data from working storage to record structure
                move ws-profile-first-name to profile-first-name
                move ws-profile-last-name to profile-last-name
                move ws-profile-university to profile-university
                move ws-profile-major to profile-major
                move ws-profile-graduation-year to profile-graduation-year
                move ws-profile-about-me to profile-about-me
                
                *> Copy experience entries
                perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
                    move ws-exp-title(profile-loop-index) to exp-title(profile-loop-index)
                    move ws-exp-company(profile-loop-index) to exp-company(profile-loop-index)
                    move ws-exp-dates(profile-loop-index) to exp-dates(profile-loop-index)
                    move ws-exp-description(profile-loop-index) to exp-description(profile-loop-index)
                end-perform
                
                *> Copy education entries
                perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
                    move ws-edu-degree(profile-loop-index) to edu-degree(profile-loop-index)
                    move ws-edu-university(profile-loop-index) to edu-university(profile-loop-index)
                    move ws-edu-years(profile-loop-index) to edu-years(profile-loop-index)
                end-perform
                
                move 'Y' to profile-initialized
                rewrite acct-record
                if acct-database-status = "00"
                    move "Profile saved successfully!" to output-buffer
                    perform outputLine
                else
                    move "Error saving profile data" to output-buffer
                    perform outputLine
                end-if
        end-read
    else
        move "Error opening database for profile save" to output-buffer
        perform outputLine
    end-if
    
    close acct-database.
    exit.

*> Paragraph: load-profile-data
*> Purpose:   Load profile data from the account database
*> Input:     current-user
*> Output:    profile data loaded into record structure
load-profile-data.
    open input acct-database.
    
    if acct-database-status = "00"
        move current-user to acct-username
        read acct-database
            key is acct-username
            invalid key
                move "Error: User not found for profile load" to output-buffer
                perform outputLine
            not invalid key
                *> Profile data is now loaded into the record structure
                if profile-empty
                    perform initialize-profile
                else
                    *> Copy profile data from record structure to working storage
                    move profile-first-name to ws-profile-first-name
                    move profile-last-name to ws-profile-last-name
                    move profile-university to ws-profile-university
                    move profile-major to ws-profile-major
                    move profile-graduation-year to ws-profile-graduation-year
                    move profile-about-me to ws-profile-about-me
                    
                    *> Copy experience entries
                    perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
                        move exp-title(profile-loop-index) to ws-exp-title(profile-loop-index)
                        move exp-company(profile-loop-index) to ws-exp-company(profile-loop-index)
                        move exp-dates(profile-loop-index) to ws-exp-dates(profile-loop-index)
                        move exp-description(profile-loop-index) to ws-exp-description(profile-loop-index)
                    end-perform
                    
                    *> Copy education entries
                    perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
                        move edu-degree(profile-loop-index) to ws-edu-degree(profile-loop-index)
                        move edu-university(profile-loop-index) to ws-edu-university(profile-loop-index)
                        move edu-years(profile-loop-index) to ws-edu-years(profile-loop-index)
                    end-perform
                    
                    move profile-initialized to ws-profile-initialized
                end-if
        end-read
    else
        move "Error opening database for profile load" to output-buffer
        perform outputLine
        perform initialize-profile
    end-if
    
    close acct-database.
    exit.

*>*******************************************************************
*> Profile creation and editing menu
*>*******************************************************************
profile-create-edit-menu.
    *> Load existing profile data
    perform load-profile-data
    
    perform outputLine
    move "--- Create/Edit Profile ---" to output-buffer
    perform outputLine
    perform outputLine
    
    *> Required fields
    move "Enter First Name:" to output-buffer
    perform outputLine
    perform readInputLine
    move function trim(input-buffer trailing) to ws-profile-first-name
    
    move "Enter Last Name:" to output-buffer
    perform outputLine
    perform readInputLine
    move function trim(input-buffer trailing) to ws-profile-last-name
    
    move "Enter University/College Attended:" to output-buffer
    perform outputLine
    perform readInputLine
    move function trim(input-buffer trailing) to ws-profile-university
    
    move "Enter Major:" to output-buffer
    perform outputLine
    perform readInputLine
    move function trim(input-buffer trailing) to ws-profile-major
    
    *> Graduation year with validation
    perform with test after until profile-valid or not valid-read
        move "Enter Graduation Year (YYYY):" to output-buffer
        perform outputLine
        perform readInputLine
        move function trim(input-buffer trailing) to profile-year-string
        perform validate-graduation-year
        if profile-invalid
            move "Invalid year. Please enter a 4-digit year (1900-2100):" to output-buffer
            perform outputLine
        end-if
    end-perform
    move profile-year-numeric to ws-profile-graduation-year
    
    *> Optional About Me
    move "Enter About Me (optional, max 200 chars, enter blank line to skip):" to output-buffer
    perform outputLine
    perform readInputLine
    if function trim(input-buffer trailing) not = spaces
        move function trim(input-buffer trailing) to ws-profile-about-me
    end-if
    
    *> Experience entries (up to 3)
    move 0 to profile-entry-count
    perform with test after until profile-entry-count >= 3 or not valid-read
        move "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" to output-buffer
        perform outputLine
        perform readInputLine
        if function trim(input-buffer trailing) = 'DONE'
            exit perform
        end-if
        add 1 to profile-entry-count
        move function trim(input-buffer trailing) to ws-exp-title(profile-entry-count)
        
        move "Experience #" to output-buffer
        string output-buffer delimited by size
               profile-entry-count delimited by size
               " - Company/Organization:" delimited by size
               into output-buffer
        end-string
        perform outputLine
        perform readInputLine
        move function trim(input-buffer trailing) to ws-exp-company(profile-entry-count)
        
        move "Experience #" to output-buffer
        string output-buffer delimited by size
               profile-entry-count delimited by size
               " - Dates (e.g., Summer 2024):" delimited by size
               into output-buffer
        end-string
        perform outputLine
        perform readInputLine
        move function trim(input-buffer trailing) to ws-exp-dates(profile-entry-count)
        
        move "Experience #" to output-buffer
        string output-buffer delimited by size
               profile-entry-count delimited by size
               " - Description (optional, max 100 chars, blank to skip):" delimited by size
               into output-buffer
        end-string
        perform outputLine
        perform readInputLine
        if function trim(input-buffer trailing) not = spaces
            move function trim(input-buffer trailing) to ws-exp-description(profile-entry-count)
        end-if
    end-perform
    
    *> Education entries (up to 3)
    move 0 to profile-entry-count
    perform with test after until profile-entry-count >= 3 or not valid-read
        move "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" to output-buffer
        perform outputLine
        perform readInputLine
        if function trim(input-buffer trailing) = 'DONE'
            exit perform
        end-if
        add 1 to profile-entry-count
        move function trim(input-buffer trailing) to ws-edu-degree(profile-entry-count)
        
        move "Education #" to output-buffer
        string output-buffer delimited by size
               profile-entry-count delimited by size
               " - University/College:" delimited by size
               into output-buffer
        end-string
        perform outputLine
        perform readInputLine
        move function trim(input-buffer trailing) to ws-edu-university(profile-entry-count)
        
        move "Education #" to output-buffer
        string output-buffer delimited by size
               profile-entry-count delimited by size
               " - Years Attended (e.g., 2023-2025):" delimited by size
               into output-buffer
        end-string
        perform outputLine
        perform readInputLine
        move function trim(input-buffer trailing) to ws-edu-years(profile-entry-count)
    end-perform
    
    *> Save profile data
    perform save-profile-data
    exit.

*>*******************************************************************
*> Profile viewing menu
*>*******************************************************************
profile-view-menu.
    *> Load existing profile data
    perform load-profile-data
    
    perform outputLine
    move "--- Your Profile ---" to output-buffer
    perform outputLine
    
    *> Display basic information
    move "Name: " to output-buffer
    string output-buffer delimited by size
           function trim(ws-profile-first-name trailing) delimited by size
           " " delimited by size
           function trim(ws-profile-last-name trailing) delimited by size
           into output-buffer
    end-string
    perform outputLine
    
    move "University: " to output-buffer
    string output-buffer delimited by size
           function trim(ws-profile-university trailing) delimited by size
           into output-buffer
    end-string
    perform outputLine
    
    move "Major: " to output-buffer
    string output-buffer delimited by size
           function trim(ws-profile-major trailing) delimited by size
           into output-buffer
    end-string
    perform outputLine
    
    move "Graduation Year: " to output-buffer
    string output-buffer delimited by size
           ws-profile-graduation-year delimited by size
           into output-buffer
    end-string
    perform outputLine
    
    *> Display About Me if present
    if function trim(ws-profile-about-me trailing) not = spaces
        move "About Me: " to output-buffer
        string output-buffer delimited by size
               function trim(ws-profile-about-me trailing) delimited by size
               into output-buffer
        end-string
        perform outputLine
    end-if
    
    *> Display Experience entries
    move 0 to profile-loop-index
    perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
        if function trim(ws-exp-title(profile-loop-index) trailing) not = spaces
            if profile-loop-index = 1
                move "Experience:" to output-buffer
                perform outputLine
            end-if
            move "Title: " to output-buffer
            string output-buffer delimited by size
                   function trim(ws-exp-title(profile-loop-index) trailing) delimited by size
                   into output-buffer
            end-string
            perform outputLine
            
            move "Company: " to output-buffer
            string output-buffer delimited by size
                   function trim(ws-exp-company(profile-loop-index) trailing) delimited by size
                   into output-buffer
            end-string
            perform outputLine
            
            move "Dates: " to output-buffer
            string output-buffer delimited by size
                   function trim(ws-exp-dates(profile-loop-index) trailing) delimited by size
                   into output-buffer
            end-string
            perform outputLine
            
            if function trim(ws-exp-description(profile-loop-index) trailing) not = spaces
                move "Description: " to output-buffer
                string output-buffer delimited by size
                       function trim(ws-exp-description(profile-loop-index) trailing) delimited by size
                       into output-buffer
                end-string
                perform outputLine
            end-if
        end-if
    end-perform
    
    *> Display Education entries
    move 0 to profile-loop-index
    perform varying profile-loop-index from 1 by 1 until profile-loop-index > 3
        if function trim(ws-edu-degree(profile-loop-index) trailing) not = spaces
            if profile-loop-index = 1
                move "Education:" to output-buffer
                perform outputLine
            end-if
            move "Degree: " to output-buffer
            string output-buffer delimited by size
                   function trim(ws-edu-degree(profile-loop-index) trailing) delimited by size
                   into output-buffer
            end-string
            perform outputLine
            
            move "University: " to output-buffer
            string output-buffer delimited by size
                   function trim(ws-edu-university(profile-loop-index) trailing) delimited by size
                   into output-buffer
            end-string
            perform outputLine
            
            move "Years: " to output-buffer
            string output-buffer delimited by size
                   function trim(ws-edu-years(profile-loop-index) trailing) delimited by size
                   into output-buffer
            end-string
            perform outputLine
        end-if
    end-perform
    
    move "--------------------" to output-buffer
    perform outputLine
    exit.

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
