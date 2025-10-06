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

       *> Pending connection requests file for persistence
       select pending-requests assign to 'pending-requests.dat'
           organization is indexed
           access mode is dynamic
           record key is req-key
           file status is pending-status.


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

*> Pending requests record
fd pending-requests.
copy "connections.cpy".

working-storage section.
*>-----pending requests file variables-----
01 pending-status          pic xx.
       88 pending-ok               value "00".
       88 pending-not-found        value "23".
       88 pending-file-missing     value "35".
01 pending-key-buffer     pic x(64).
01 pending-alt-key        pic x(64).
01 pending-sender         pic x(30).
01 pending-recipient      pic x(30).
01 pending-count          pic 9(4) value 0.

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
01 temp-current-user pic x(30).

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
01 post-login-1 constant as "[1] Create/Edit My Profile".
01 post-login-2 constant as "[2] View My Profile".
01 post-login-3 constant as "[3] Search for User".
01 post-login-4 constant as "[4] Learn a New Skill".
01 post-login-5 constant as "[5] Job search/internship".

01 post-login-6 constant as "[6] View My Pending Connection Requests".
01 send-conn-1  constant as "[1] Send Connection Request".
01 send-conn-2  constant as "[2] Back to Main Menu".
01 conn-sent-prefix constant as "Connection request sent to ".
01 conn-dup-msg constant as "You have already sent a connection request to this user.".
01 conn-recv-pending constant as "This user has already sent you a connection request.".
01 conn-invalid-msg constant as "You cannot send a request to yourself.".
01 pending-title constant as "--- Pending Connection Requests ---".
01 pending-empty constant as "You have no pending connection requests at this time.".
01 conn-choice-prompt constant as "Enter your choice:".

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

*> Profile management constants
01 profile-create-title constant as "--- Create/Edit Profile ---".
01 PROFILE-VIEW-TITLE   constant as "--- User Profile ---".
01 display_profile      constant as "Displaying profile...".
01 profile-saved-msg    constant as "Profile saved successfully!".
01 profile-separator    constant as "--------------------".
01 profile-name-prefix  constant as "Name: ".
01 profile-univ-prefix  constant as "University: ".
01 profile-major-prefix constant as "Major: ".
01 profile-year-prefix  constant as "Graduation Year: ".
01 profile-about-prefix constant as "About Me: ".
01 profile-exp-prefix   constant as "Experience:".
01 profile-edu-prefix   constant as "Education:".
01 profile-title-prefix constant as "Title: ".
01 profile-company-prefix constant as "Company: ".
01 profile-dates-prefix constant as "Dates: ".
01 profile-desc-prefix  constant as "Description: ".
01 profile-degree-prefix constant as "Degree: ".
01 profile-years-prefix constant as "Years: ".

*> Profile input prompts
01 profile-first-name-prompt constant as "Enter First Name:".
01 profile-last-name-prompt constant as "Enter Last Name:".
01 profile-university-prompt constant as "Enter University/College Attended:".
01 profile-major-prompt constant as "Enter Major:".
01 profile-graduation-prompt constant as "Enter Graduation Year (YYYY):".
01 profile-about-prompt constant as "Enter About Me (optional, max 200 chars, enter blank line to skip):".
01 profile-exp-prompt constant as "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):".
01 profile-edu-prompt constant as "Add Education (optional, max 3 entries. Enter 'DONE' to finish):".

*> Profile validation working variables
01 profile-validation pic x(1).
       88 profile-valid value 'Y'.
       88 profile-invalid value 'N'.
01 graduation-year-num pic 9(4).
01 current-year pic 9(4) value 2024.
01 min-graduation-year pic 9(4) value 1950.
01 max-graduation-year pic 9(4) value 2100.
01 profile-input-buffer pic x(200).
01 profile-counter pic 9(1).
01 profile-done-flag pic x(1).
       88 profile-done value 'Y'.
       88 profile-continue value 'N'.

*> Name search variables
01 buffer-first-name pic x(50).
01 buffer-last-name pic x(50).

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
           move choice-prompt to input-prompt
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
           move post-login-5 to output-buffer
           perform outputLine
           move post-login-6 to output-buffer
           perform outputLine
           move logout to output-buffer
           perform outputLine
           move choice-prompt to input-prompt
           perform readInputLine
           move input-buffer(1:1) to menu-choice

           evaluate true
               when menu-choice = '1'
                   perform create-edit-profile
               when menu-choice = '2'
                   perform view-profile
               when menu-choice = '3'
                   perform searchUserProfile
               when menu-choice = '4'
                   perform skills-menu
               when menu-choice = '5'
               *> Job search/internship under construction
                   move spaces to output-buffer
                   string uc-job-prefix delimited by size
                          under-construction delimited by size
                          into output-buffer
                   end-string
                   perform outputLine
               when menu-choice = '6'
                    perform viewPendingRequests
                when menu-choice = 'q' or not valid-read
                   exit perform
               when other
                   move "Invalid choice. Please try again." to output-buffer
                   perform outputLine
           end-evaluate
       end-perform
       exit.


*>*******************************************************************
*> Profile Management Procedures
*>*******************************************************************

*>*******************************************************************
*> Create or Edit Profile
*>*******************************************************************
create-edit-profile.
       move profile-create-title to output-buffer
       perform outputLine

       *> Load existing profile if it exists
       move current-user to buffer-acct-username
       perform findAcct
       if acct-found
           *> Sucessfully retrieved account
           continue
       else
           *> User somehow does not exist in database yet is signed in
           continue
       end-if

       *> Get required profile information
       perform get-required-profile-info

       *> Get optional profile information
       perform get-optional-profile-info

       *> Save profile
       perform save-profile

       move profile-saved-msg to output-buffer
       perform outputLine
       exit.

*>*******************************************************************
*> Get Required Profile Information
*>*******************************************************************
get-required-profile-info.
       *> First Name
       move 'N' to profile-validation
       perform until profile-valid or not valid-read
           move profile-first-name-prompt to output-buffer
           perform outputLine
           perform readInputLine

           if input-buffer not equal to spaces
               move 'Y' to profile-validation
               move function trim(input-buffer trailing) to profile-first-name
           end-if
       end-perform

       *> Last Name
       move 'N' to profile-validation
       perform until profile-valid or not valid-read
           move profile-last-name-prompt to output-buffer
           perform outputLine
           perform readInputLine

           if input-buffer not equal to spaces
               move 'Y' to profile-validation
               move function trim(input-buffer trailing) to profile-last-name
           end-if
       end-perform

       *> University
       move 'N' to profile-validation
       perform until profile-valid or not valid-read
           move profile-university-prompt to output-buffer
           perform outputLine
           perform readInputLine

           if input-buffer not equal to spaces
               move 'Y' to profile-validation
               move function trim(input-buffer trailing) to profile-university
           end-if
       end-perform

       *> Major
       move 'N' to profile-validation
       perform until profile-valid or not valid-read
           move profile-major-prompt to output-buffer
           perform outputLine
           perform readInputLine

           if input-buffer not equal to spaces
               move 'Y' to profile-validation
               move function trim(input-buffer trailing) to profile-major
           end-if
       end-perform

       *> Graduation Year with validation
       move 'N' to profile-validation
       perform until profile-valid or not valid-read
           move profile-graduation-prompt to output-buffer
           perform outputLine
           perform readInputLine
           move function trim(input-buffer trailing) to profile-input-buffer

           perform validate-graduation-year
           if not profile-valid
               move "Invalid graduation year. Please enter a valid 4-digit year." to output-buffer
               perform outputLine
           end-if
       end-perform
       exit.

*>*******************************************************************
*> Get Optional Profile Information
*>*******************************************************************
get-optional-profile-info.
       *> About Me
       move profile-about-prompt to output-buffer
       perform outputLine
       perform readInputLine
       if function trim(input-buffer trailing) = spaces
           move spaces to profile-about-me
       else
           move function trim(input-buffer trailing) to profile-about-me
       end-if

       *> Experience entries
       move 1 to profile-counter
       move 'N' to profile-done-flag
       perform until profile-done or profile-counter > 3 or not valid-read
           move spaces to output-buffer
           string "Experience #" delimited by size
                  profile-counter delimited by size
                  " - Title:" delimited by size
                  into output-buffer
           end-string
           perform outputLine
           perform readInputLine

           if function trim(input-buffer trailing) = 'DONE' or
              function trim(input-buffer trailing) = 'done'
               move 'Y' to profile-done-flag
           else
               move function trim(input-buffer trailing) to exp-title(profile-counter)

               move spaces to output-buffer
               string "Experience #" delimited by size
                      profile-counter delimited by size
                      " - Company/Organization:" delimited by size
                      into output-buffer
               end-string
               perform outputLine
               perform readInputLine
               move function trim(input-buffer trailing) to exp-company(profile-counter)

               move spaces to output-buffer
               string "Experience #" delimited by size
                      profile-counter delimited by size
                      " - Dates (e.g., Summer 2024):" delimited by size
                      into output-buffer
               end-string
               perform outputLine
               perform readInputLine
               move function trim(input-buffer trailing) to exp-dates(profile-counter)

               move spaces to output-buffer
               string "Experience #" delimited by size
                      profile-counter delimited by size
                      " - Description (optional, max 100 chars, blank to skip):" delimited by size
                      into output-buffer
               end-string
               perform outputLine
               perform readInputLine
               if function trim(input-buffer trailing) = spaces
                   move spaces to exp-description(profile-counter)
               else
                   move function trim(input-buffer trailing) to exp-description(profile-counter)
               end-if

               add 1 to profile-counter
           end-if
       end-perform

       *> Education entries
       move 1 to profile-counter
       move 'N' to profile-done-flag
       perform until profile-done or profile-counter > 3 or not valid-read
           move spaces to output-buffer
           string "Education #" delimited by size
                  profile-counter delimited by size
                  " - Degree:" delimited by size
                  into output-buffer
           end-string
           perform outputLine
           perform readInputLine

           if function trim(input-buffer trailing) = 'DONE' or
              function trim(input-buffer trailing) = 'done'
               move 'Y' to profile-done-flag
           else
               move function trim(input-buffer trailing) to edu-degree(profile-counter)

               move spaces to output-buffer
               string "Education #" delimited by size
                      profile-counter delimited by size
                      " - University/College:" delimited by size
                      into output-buffer
               end-string
               perform outputLine
               perform readInputLine
               move function trim(input-buffer trailing) to edu-university(profile-counter)

               move spaces to output-buffer
               string "Education #" delimited by size
                      profile-counter delimited by size
                      " - Years Attended (e.g., 2023-2025):" delimited by size
                      into output-buffer
               end-string
               perform outputLine
               perform readInputLine
               move function trim(input-buffer trailing) to edu-years(profile-counter)

               add 1 to profile-counter
           end-if
       end-perform
       exit.

*>*******************************************************************
*> Validate Graduation Year
*>*******************************************************************
validate-graduation-year.
       move 'N' to profile-validation

       *> Check if input is numeric and 4 digits
       if function length(function trim(profile-input-buffer trailing)) = 4
           move profile-input-buffer to graduation-year-num
           if graduation-year-num >= min-graduation-year and
              graduation-year-num <= max-graduation-year
               move graduation-year-num to profile-graduation-year
               move 'Y' to profile-validation
           end-if
       end-if
       exit.

*>*******************************************************************
*> Save Profile
*>*******************************************************************
save-profile.
       move 'Y' to profile-has-data
       move current-user to acct-username
       perform updateAcct
       exit.

*>*******************************************************************
*> View Profile
*>*******************************************************************
view-profile.
       *> Load profile data
       move current-user to buffer-acct-username
       perform findAcct

       if acct-found and function trim(profile-first-name trailing) not = spaces
           move display_profile to output-buffer
           perform outputLine
           move profile-view-title to output-buffer
           perform outputLine

           *> Display basic information
           move spaces to output-buffer
           string profile-name-prefix delimited by size
                  function trim(profile-first-name trailing) delimited by size
                  " " delimited by size
                  function trim(profile-last-name trailing) delimited by size
                  into output-buffer
           end-string
           perform outputLine

           move spaces to output-buffer
           string profile-univ-prefix delimited by size
                  function trim(profile-university trailing) delimited by size
                  into output-buffer
           end-string
           perform outputLine

           move spaces to output-buffer
           string profile-major-prefix delimited by size
                  function trim(profile-major trailing) delimited by size
                  into output-buffer
           end-string
           perform outputLine

           move spaces to output-buffer
           string profile-year-prefix delimited by size
                  profile-graduation-year delimited by size
                  into output-buffer
           end-string
           perform outputLine

           *> Display About Me if present
           if function trim(profile-about-me trailing) not = spaces
               move spaces to output-buffer
               string profile-about-prefix delimited by size
                      function trim(profile-about-me trailing) delimited by size
                      into output-buffer
               end-string
               perform outputLine
           end-if

           *> Display Experience
           move profile-exp-prefix to output-buffer
           perform outputLine
           perform varying profile-counter from 1 by 1 until profile-counter > 3
               if function trim(exp-title(profile-counter) trailing) not = spaces
                   move spaces to output-buffer
                   string profile-title-prefix delimited by size
                          function trim(exp-title(profile-counter) trailing) delimited by size
                          into output-buffer
                   end-string
                   perform outputLine

                   move spaces to output-buffer
                   string profile-company-prefix delimited by size
                          function trim(exp-company(profile-counter) trailing) delimited by size
                          into output-buffer
                   end-string
                   perform outputLine

                   move spaces to output-buffer
                   string profile-dates-prefix delimited by size
                          function trim(exp-dates(profile-counter) trailing) delimited by size
                          into output-buffer
                   end-string
                   perform outputLine

                   if function trim(exp-description(profile-counter) trailing) not = spaces
                       move spaces to output-buffer
                       string profile-desc-prefix delimited by size
                              function trim(exp-description(profile-counter) trailing) delimited by size
                              into output-buffer
                       end-string
                       perform outputLine
                   end-if
               end-if
           end-perform

           *> Display Education
           move profile-edu-prefix to output-buffer
           perform outputLine
           perform varying profile-counter from 1 by 1 until profile-counter > 3
               if function trim(edu-degree(profile-counter) trailing) not = spaces
                   move spaces to output-buffer
                   string profile-degree-prefix delimited by size
                          function trim(edu-degree(profile-counter) trailing) delimited by size
                          into output-buffer
                   end-string
                   perform outputLine

                   move spaces to output-buffer
                   string profile-univ-prefix delimited by size
                          function trim(edu-university(profile-counter) trailing) delimited by size
                          into output-buffer
                   end-string
                   perform outputLine

                   move spaces to output-buffer
                   string profile-years-prefix delimited by size
                          function trim(edu-years(profile-counter) trailing) delimited by size
                          into output-buffer
                   end-string
                   perform outputLine
               end-if
           end-perform

           move profile-separator to output-buffer
           perform outputLine
       else
           move "No profile found. Please create a profile first." to output-buffer
           perform outputLine
       end-if
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
           move choice-prompt to input-prompt
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
           initialize acct-record
           move "N" to profile-has-data

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

*>*******************************************************************
*> Update Account with Profile Data
*>*******************************************************************
updateAcct.
       open i-o acct-database.

       if acct-database-status = "00"
           move current-user to acct-username
           rewrite acct-record
           if acct-database-status not = "00"
               string
                   "Error updating account: " delimited by size
                   acct-database-status delimited by size
                   into output-buffer
               end-string
               perform outputLine
           end-if
       else
           string
               "Error opening account database for update: " delimited by size
               acct-database-status delimited by size
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
*> Find Profile by First and Last Name
*> Input: buffer-first-name, buffer-last-name
*> Output: Sets acct-status and loads profile data if found
*>*******************************************************************
findProfile.
       open input acct-database.

       move '2' to acct-status  *> Initialize as not found

       if acct-database-status = "00"
           *> Sequential search through all records to find name match
           perform until not database-good-read or acct-found
               read acct-database next record
                   at end
                       exit perform
                   not at end
                       *> Check if first and last names match
                       if function trim(profile-first-name trailing) =
                          function trim(buffer-first-name trailing) and
                          function trim(profile-last-name trailing) =
                          function trim(buffer-last-name trailing)
                           move '1' to acct-status
                           move acct-username to buffer-acct-username
                           exit perform
                       end-if
               end-read
           end-perform
       else
           if database-does-not-exist
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
*> Search User by Name - Uses findProfile function
*> Input: Takes first and last name input from user
*> Output: Displays profile if found, error message if not
*>*******************************************************************
searchUserProfile.
       move "Enter the first name to search for:" to output-buffer
       perform outputLine
       perform readInputLine
       move function trim(input-buffer trailing) to buffer-first-name

       move "Enter the last name to search for:" to output-buffer
       perform outputLine
       perform readInputLine
       move function trim(input-buffer trailing) to buffer-last-name

       *> Use new findProfile module
       perform findProfile

       *> Check result and display appropriate response
       if acct-found
           move "User found!" to output-buffer
           perform outputLine

           *> Check if user has a profile created
           if profile-exists
               *> Temporarily store current user and switch to searched user
               move current-user to temp-current-user
               move buffer-acct-username to current-user

               *> Use existing view-profile function
               perform view-profile
               *> EPIC4 SEND-REQUEST (additive)
               move send-conn-1 to output-buffer
               perform outputLine
               move send-conn-2 to output-buffer
               perform outputLine
               move conn-choice-prompt to input-prompt
               perform readInputLine
               move input-buffer(1:1) to menu-choice
               if menu-choice = '1'
                   move temp-current-user to pending-sender
                   move buffer-acct-username to pending-recipient
                   perform sendConnectionRequest
               end-if

               *> Restore original current user
               move temp-current-user to current-user
           else
               move "This user has not created a profile yet." to output-buffer
               perform outputLine
           end-if
       else
           move "User not found." to output-buffer
           perform outputLine
       end-if
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


*>*******************************************************************
*> View list of pending connection requests for current user
*>*******************************************************************
viewPendingRequests.
       move pending-title to output-buffer
       perform outputLine
       move 0 to pending-count

       open input pending-requests
       if pending-status = "00"
           perform until pending-status not = "00"
               read pending-requests next record
                   at end
                       exit perform
                   not at end
                       if function trim(req-recipient trailing) = function trim(current-user trailing)
                           add 1 to pending-count
                           move spaces to output-buffer
                           string "- " delimited by size
                                  function trim(req-sender trailing) delimited by size
                                  into output-buffer
                           end-string
                           perform outputLine
                       end-if
               end-read
           end-perform
       end-if
       close pending-requests

       if pending-count = 0
           move pending-empty to output-buffer
           perform outputLine
       end-if

       move "--------------------" to output-buffer
       perform outputLine

       *> Menu input (username to prompt if you want to accept or deny)
       *> q for quit
       move "Enter the username of the user you wish to connect with (q to quit)" to output-buffer
       perform outputLine.
       move "> " to input-prompt
       perform readInputLine

       *>input-buffer
       *> Check if input-buffer is not q
       *> If it isn't, call the request processing menu
           *> Search for connection request and ensure it exists
           *> If profile found, then show connection request processing menu
       *> If q, do nothing

       evaluate true
          when input-buffer = 'q'
              continue
          when other
              perform requestProcessingMenu
      end-evaluate

      exit.

requestProcessingMenu.
       *> Check if connection request exists
           *> If it doesn't, output that to use
           *> If it does, display menu options
                *> If user chooses to accept
                   *> Call makeConnection function to save into file
                   *> Call delete pending request function
                   *> Display message to user
                *> If user chooses to reject connection
                     *> Call delete pending request function
                     *> Display message to user
       exit.


*>*******************************************************************
*> Send a connection request from pending-sender to pending-recipient
*>*******************************************************************
sendConnectionRequest.
       if function trim(pending-sender trailing) = function trim(pending-recipient trailing)
           move conn-invalid-msg to output-buffer
           perform outputLine
           move "--------------------" to output-buffer
           perform outputLine
           exit paragraph
       end-if

*> Validate that recipient account exists
       move function trim(pending-recipient trailing) to buffer-acct-username
       perform findAcct
       if acct-status not = '1'
           move "No such user found." to output-buffer
           perform outputLine
           move "--------------------" to output-buffer
           perform outputLine
           exit paragraph
       end-if

       *> Build keys: primary (recipient|sender) and alternate (sender|recipient)
       move spaces to pending-key-buffer
       move spaces to pending-alt-key
       string function trim(pending-recipient trailing) delimited by size
              '|' delimited by size
              function trim(pending-sender trailing) delimited by size
              into pending-key-buffer
       end-string
       string function trim(pending-sender trailing) delimited by size
              '|' delimited by size
              function trim(pending-recipient trailing) delimited by size
              into pending-alt-key
       end-string

       open i-o pending-requests
       if pending-status = "35"
           open output pending-requests
           close pending-requests
           open i-o pending-requests
       end-if

       if pending-status = "00"
           *> Check if duplicate request already exists (you -> them)
           move pending-key-buffer to req-key
           read pending-requests
               key is req-key
               invalid key
                   continue
               not invalid key
                   move conn-dup-msg to output-buffer
                   perform outputLine
                   close pending-requests
                   move "--------------------" to output-buffer
                   perform outputLine
                   exit paragraph
           end-read

           *> Check if recipient already sent you a request (them -> you)
           move pending-alt-key to req-key
           read pending-requests
               key is req-key
               invalid key
                   continue
               not invalid key
                   move conn-recv-pending to output-buffer
                   perform outputLine
                   close pending-requests
                   move "--------------------" to output-buffer
                   perform outputLine
                   exit paragraph
           end-read

           *> Write new pending record
           move pending-key-buffer to req-key
           move function trim(pending-recipient trailing) to req-recipient
           move function trim(pending-sender trailing)    to req-sender
           write pending-record
           move spaces to output-buffer
           string conn-sent-prefix delimited by size
                  function trim(pending-recipient trailing) delimited by size
                  '.' delimited by size
                  into output-buffer
           end-string
           perform outputLine
           move "--------------------" to output-buffer
           perform outputLine
       else
           move "Error opening pending requests file." to output-buffer
           perform outputLine
       end-if
       close pending-requests
       exit.
