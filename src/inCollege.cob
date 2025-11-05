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

       *> Established connections file
           select connection-database assign to 'connection-database.dat'
               organization is indexed
               access mode is dynamic
               record key is connection-key
               file status is connection-database-status.

       *> Job Listing File
           select job-database assign to 'job-database.dat'
               organization is indexed
               access mode is dynamic
               record key is job-key
               file status is job-database-status.
       *> Applications database (persistence for job applications)
           select application-database assign to 'application-database.dat'
               organization is indexed
               access mode is dynamic
               record key is application-key
               file status is application-database-status.

       *> Message database for persistent messaging
           select message-database assign to 'message-database.dat'
               organization is indexed
               access mode is dynamic
               record key is message-key
               file status is message-database-status.


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
       copy "req-connections.cpy".

*> Connections database record
       fd connection-database.
       copy "connections.cpy". *>replacing ==req== by ==connection==.

*> Job database record
       fd job-database.
       copy "job.cpy".

    *> Applications persistence — use copybook for consistency with other FDs
       fd application-database.
           copy "application.cpy".

*> Message database record
       fd message-database.
       copy "message.cpy".

       working-storage section.
*>-----pending requests file variables-----
       01 pending-status          pic xx.
              88 pending-ok               value "00".
              88 pending-not-found        value "23".
              88 pending-file-missing     value "35".
       01 pending-key-buffer     pic x(204).
       01 pending-alt-key        pic x(204).
       01 pending-sender         pic x(100).
       01 pending-recipient      pic x(100).
       01 pending-count          pic 9(4) value 0.
       01 pending-searched-user  pic x(100).

*>-----connection file variables-----
       01 connection-database-status pic xx.
              88 connection-ok               value "00".
              88 connection-not-found        value "23".
              88 connection-file-missing     value "35".
       01 connection-count                 pic 9(4) value 0.
       01 connection-other                 pic x(30).

*>-----user-connection-variables-----
       01 connection-status pic xx.
              88 connected               value "00".
              88 not-connected        value "23".

*>-----job file variables-----
       01 job-database-status pic xx.
              88 job-database-ok               value "00".
              88 job-not-found        value "23".
              88 job-file-missing     value "35".

*>-----application file variables-----
       01 application-database-status pic xx.
           88 application-ok               value "00".
           88 application-not-found        value "23".
           88 application-file-missing     value "35".
       01 application-key-buffer pic x(64).
       01 application-job-key-buffer pic x(200).
       01 application-job-title-buffer pic x(100).
       01 application-employer-buffer pic x(100).
       01 application-location-buffer pic x(100).
       01 application-counter pic 9(4) value 0.
       01 application-id-seq pic 9(9) value 1.
       01 browse-choice pic x(1).
       01 browse-index pic 9(4) value 0.
       01 selected-job-index pic 9(4) value 0.
       01 browse-done-flag pic x(1) value 'N'.
            88 browse-done value 'Y'.
            88 browse-continue value 'N'.

*>-----message file variables-----
       01 message-database-status pic xx.
           88 message-ok               value "00".
           88 message-not-found        value "23".
           88 message-file-missing     value "35".
       01 message-key-buffer pic x(64).
       01 message-sender-buffer pic x(100).
       01 message-recipient-buffer pic x(100).
       01 message-content-buffer pic x(500).
       01 message-counter pic 9(4) value 0.
       01 message-id-seq pic 9(9) value 1.

*> In-memory job listing tables for browse selection
       01 job-list-keys.
           05 job-key-entry occurs 100 pic x(200).
       01 job-list-titles.
           05 job-title-entry occurs 100 pic x(100).
       01 job-list-employers.
           05 job-employer-entry occurs 100 pic x(100).
       01 job-list-locations.
           05 job-location-entry occurs 100 pic x(100).


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
       01 conn-dup-request-msg constant as "You have already sent a connection request to this user.".
       01 conn-recv-pending constant as "This user has already sent you a connection request.".
       01 conn-dup-msg constant as "You are already connected with this user".
       01 conn-invalid-msg constant as "You cannot send a request to yourself.".
       01 pending-title constant as "--- Pending Connection Requests ---".
       01 pending-empty constant as "You have no pending connection requests at this time.".
       01 conn-choice-prompt constant as "Enter your choice:".
       01 post-login-7 constant as "[7] View My Connections".
       01 connections-title constant as "--- My Connections ---".
       01 connections-empty constant as "You have no connections".

       01 post-login-8 constant as "[8] Messages".
       01 messages-title constant as "--- Messages ---".
       01 messages-menu-1 constant as "[1] Send a New Message".
       01 messages-menu-2 constant as "[2] View My Messages".
       01 messages-back constant as "[q] Back to Main Menu".
       01 message-sent-msg constant as "Message sent successfully!".
       01 message-recipient-prompt constant as "Enter the username of the recipient:".
       01 message-content-prompt constant as "Enter your message:".
       01 message-not-connected constant as "User not found in your network.".
       01 message-user-not-found constant as "User not found in your network.".
       01 view-messages-uc constant as "View My Messages is under construction.".

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
                  move post-login-7 to output-buffer
                  perform outputLine
                  move post-login-8 to output-buffer
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
                          perform jobSearch
                      when menu-choice = '6'
                           perform viewPendingRequests
                     when menu-choice = '7'
                          perform viewConnections
                      when menu-choice = '8'
                          perform messaging-menu
                       when menu-choice = 'q' or not valid-read
                          exit perform
                      when other
                          move "Invalid choice. Please try again." to output-buffer
                          perform outputLine
                  end-evaluate
              end-perform
              exit.


*>*******************************************************************
*> View established connections for current user
*>*******************************************************************
       viewConnections.
              move connections-title to output-buffer
              perform outputLine
              move 0 to connection-count

              open input connection-database
              if connection-database-status = "00"
                  perform until connection-database-status not = "00"
                      read connection-database next record
                          at end
                              exit perform
                          not at end
                              if function trim(connection-user-1 trailing) = function trim(current-user trailing) or
                                 function trim(connection-user-2 trailing) = function trim(current-user trailing)
                                  if function trim(connection-user-1 trailing) = function trim(current-user trailing)
                                      move connection-user-2 to connection-other
                                  else
                                      move connection-user-1 to connection-other
                                  end-if
                                  add 1 to connection-count

                                  move spaces to output-buffer
                                  string "- " delimited by size
                                         function trim(connection-other trailing) delimited by size
                                         into output-buffer
                                  end-string
                                  perform outputLine

                                  *> connection-other username of connection
                                  *> Need to search up profile to get first and last name
                                  move connection-other to buffer-acct-username
                                  perform findAcct

                                  string
                                      "    " delimited by size
                                      function trim(profile-first-name trailing) delimited by size
                                      " "
                                      function trim(profile-last-name trailing) delimited by size
                                      into output-buffer
                                  end-string
                                  perform outputLine

                                 string
                                      "    " delimited by size
                                      function trim(profile-major trailing) delimited by size
                                      ", "
                                      function trim(profile-university trailing) delimited by size
                                      into output-buffer
                                  end-string
                                  perform outputLine
                              end-if
                      end-read
                  end-perform
              end-if
              close connection-database

              if connection-count = 0
                  move connections-empty to output-buffer
                  perform outputLine
              end-if

              move "--------------------" to output-buffer
              perform outputLine
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
              move "                                                               \______/                      " to output-buffer.
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
              perform outputLine
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

                                  move req-sender to buffer-acct-username
                                  perform findAcct

                                  string
                                      "    " delimited by size
                                      function trim(profile-first-name trailing) delimited by size
                                      " "
                                      function trim(profile-last-name trailing) delimited by size
                                      into output-buffer
                                  end-string
                                  perform outputLine


                                 string
                                      "    " delimited by size
                                      function trim(profile-major trailing) delimited by size
                                      ", "
                                      function trim(profile-university trailing) delimited by size
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
                  move "--------------------" to output-buffer
                  perform outputLine
              else
                  move "--------------------" to output-buffer
                  perform outputLine
                  move "Enter the username of the connection request you wish to respond to (q to quit)" to output-buffer
                  perform outputLine
                  move "> " to input-prompt
                  perform readInputLine
                  move input-buffer to pending-searched-user

                  evaluate true
                  when pending-searched-user = 'q'
                         continue
                  when other
                         perform processPendingRequests
                  end-evaluate
              end-if

             perform outputLine

            exit.

       processPendingRequests.
              open i-o pending-requests
              if pending-status = "00"
                  perform until pending-status not = "00"
                      read pending-requests next record
                          at end
                              *> Tell user that request not found
                              string
                                  "Connection request from user " delimited by size
                                  function trim(pending-searched-user trailing) delimited by size
                                  " was not found" delimited by size
                                  into output-buffer
                              end-string
                              perform outputLine
                              exit perform
                          not at end
                              if function trim(req-recipient trailing) = function trim(current-user trailing)
                              and function trim(req-sender trailing) = function trim(pending-searched-user)
                                  *> Menu asking to accept or reject connection
                                  move "[0] Connect with user" to output-buffer
                                  perform outputLine
                                  move "[1] Reject request" to output-buffer
                                  perform outputLine
                                  move "[q] Quit" to output-buffer
                                  perform outputLine

                                  move "> " to input-prompt
                                  perform readInputLine

                                  evaluate true
                                      when input-buffer = '0'
                                          *> call paragraph to save to established connections database
                                          move req-key to connection-key
                                          move req-recipient to connection-user-1
                                          move req-sender to connection-user-2

                                          perform createConnection

                                          if connection-ok
                                          *> Then delete from old database
                                              delete pending-requests
                                                  invalid key
                                                      move "Failed to accept request" to output-buffer
                                                      perform outputLine
                                                  not invalid key
                                                      move "Sucessfully accepted connection request!" to output-buffer
                                                      perform outputLine
                                              end-delete
                                          end-if
                                          exit perform
                                      when input-buffer = '1'
                                          *> Delete from pending database
                                          delete pending-requests
                                              invalid key
                                                  move "Could not reject request" to output-buffer
                                                  perform outputLine
                                              not invalid key
                                                  move "Sucessfully rejected request" to output-buffer
                                                  perform outputLine
                                          end-delete
                                          exit perform
                                      when other
                                          exit perform
                                  end-evaluate
                              end-if
                      end-read
                  end-perform
              end-if

              close pending-requests
              exit.

       createConnection.
              *> Open connection database file
              open i-o connection-database
              if connection-database-status = "35"
                  open output connection-database
                  close connection-database
                  open i-o connection-database
              end-if
              *> Store in file
              write connection-record.
              *> Close connection database file
              close connection-database
              if not connection-ok
                  move "Failed to create connection" to output-buffer
                  perform outputLine
              end-if

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
                         move conn-dup-request-msg to output-buffer
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


                  *> Check if you two are already connected!
                  open i-o connection-database
                  if connection-ok
                      move pending-key-buffer to connection-key
                      read connection-database
                          key is connection-key
                          invalid key
                              continue
                          not invalid key
                              move conn-dup-msg to output-buffer
                              perform outputLine
                              close pending-requests
                              close connection-database
                              move "--------------------" to output-buffer
                              perform outputLine
                              exit paragraph
                      end-read

                      move pending-alt-key to connection-key
                      read connection-database
                          key is connection-key
                          invalid key
                              continue
                          not invalid key
                              move conn-dup-msg to output-buffer
                              perform outputLine
                              close pending-requests
                              close connection-database
                              move "--------------------" to output-buffer
                              perform outputLine
                              exit paragraph
                      end-read
                  end-if
                  close connection-database


                  *> Write new pending record
                  move pending-key-buffer to req-key
                  move function trim(pending-recipient trailing) to req-recipient
                  move function trim(pending-sender trailing)    to req-sender
                  write req-record
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

       jobSearch.
              move "[0] Browse Jobs/Internships" to output-buffer.
              perform outputLine.
              move "[1] Post a Job/Internship" to output-buffer.
              perform outputLine.
           move "[2] View My Applications" to output-buffer.
           perform outputLine.
           move "[q] Back" to output-buffer.
              perform outputLine.

              move ">" to input-prompt.
              perform readInputLine.
              *>move input-buffer(1:1) to menu-choice

              evaluate true
                  when input-buffer = '0'
                      perform browseJobs
                  when input-buffer = '1'
                      perform createJobListing
                  when input-buffer = '2'
                      perform viewMyApplications
                  when other
                      continue
              end-evaluate

              exit.


       createJobListing.
              open i-o job-database
              if job-database-status = "35"
                  open output job-database
                  close job-database
                  open i-o job-database
              end-if

              if job-database-ok
                  *> Get Job Title (Loop)
                  move 'N' to profile-validation
                  perform until profile-valid or not valid-read
                      move "Enter Job Title (Required): " to input-prompt
                      perform readInputLine
                      if input-buffer not equal to spaces
                          move 'Y' to profile-validation
                          move function trim(input-buffer trailing) to job-title
                      end-if
                  end-perform

                  *> Get Description
                  move 'N' to profile-validation
                  perform until profile-valid or not valid-read
                      move "Enter Job Description (Required): " to input-prompt
                      perform readInputLine
                      if input-buffer not equal to spaces
                          move 'Y' to profile-validation
                           move function trim(input-buffer trailing) to job-description
                      end-if
                  end-perform

                  *> Get Employer (loop)
                  move 'N' to profile-validation
                  perform until profile-valid or not valid-read
                      move "Enter Employer (Required): " to input-prompt
                      perform readInputLine
                      if input-buffer not equal to spaces
                          move 'Y' to profile-validation
                          move function trim(input-buffer trailing) to job-employer
                      end-if
                  end-perform

                  *> Get Location
                  move 'N' to profile-validation
                  perform until profile-valid or not valid-read
                      move "Enter Job Location (Required): " to input-prompt
                      perform readInputLine
                      if input-buffer not equal to spaces
                          move 'Y' to profile-validation
                          move function trim(input-buffer trailing) to job-location
                      end-if
                  end-perform

                  *> Get Salary (Optional)
                  move "Enter salary: " to input-prompt
                  perform readInputLine
                  move function trim(input-buffer trailing) to job-salary

                  move current-user to job-creator

                  string
                      function trim(job-creator trailing) delimited by size
                      function trim(job-title trailing) delimited by size
                  into job-key

                  write job-record

                  if job-database-ok
                      move "Job posted successfully!" to output-buffer
                      perform outputLine
                  else
                      move "Failed to post job" to output-buffer
                      perform outputLine
                  end-if
              else
                  move "Job database failed to open :O" to output-buffer
                  perform outputLine
              end-if

              close job-database.
              exit.

*>*******************************************************************
*> Browse available jobs and view details
*>*******************************************************************
       browseJobs.
              *> Browse loop
              move 'N' to browse-done-flag
              perform until browse-done or not valid-read
                  move "--- Available Job Listings ---" to output-buffer
                  perform outputLine
                  move 0 to browse-index

                  open input job-database
                  if job-database-ok
                      perform until job-database-status not = "00"
                          read job-database next record
                              at end
                                  exit perform
                              not at end
                                  add 1 to browse-index
                                  *> store mapping - ensure keys are properly stored
                                  move function trim(job-key trailing) to job-key-entry(browse-index)
                                  move job-title to job-title-entry(browse-index)
                                  move job-employer to job-employer-entry(browse-index)
                                  move job-location to job-location-entry(browse-index)

                                  move spaces to output-buffer
                                  string "[" delimited by size
                                         function trim(browse-index leading) delimited by size
                                         "] " delimited by size
                                         function trim(job-title trailing) delimited by size
                                         " at " delimited by size
                                         function trim(job-employer trailing) delimited by size
                                         " (" delimited by size
                                         function trim(job-location trailing) delimited by size
                                         ")" delimited by size
                                         into output-buffer
                                  end-string
                                  perform outputLine
                          end-read
                      end-perform
                  else
                      move "Job database is not available." to output-buffer
                      perform outputLine
                  end-if
                  close job-database

                  move "Enter number to view details (q to go back):" to output-buffer
                  perform outputLine
                  move "> " to input-prompt
                  perform readInputLine
                  if input-buffer = 'q' or not valid-read
                      move 'Y' to browse-done-flag
                  else
                      *> Convert numeric entry to index by direct move
                      move function trim(input-buffer trailing) to selected-job-index
                      if selected-job-index >= 1 and selected-job-index <= browse-index
                          *> Move exact key without any trimming to preserve full value
                          move job-key-entry(selected-job-index) to application-job-key-buffer
                          perform viewJobDetails
                      else
                          move "Invalid selection." to output-buffer
                          perform outputLine
                      end-if
                  end-if
              end-perform
              exit.

*>*******************************************************************
*> View full details for a single job
*> Input: application-job-key-buffer (job-key)
*>*******************************************************************
       viewJobDetails.
              *> Ensure key is properly trimmed for lookup
              move function trim(application-job-key-buffer trailing) to job-key
              open input job-database
              if job-database-ok
                  read job-database key is job-key
                      invalid key
                          move "Job not found." to output-buffer
                          perform outputLine
                      not invalid key
                          *> Display details
                                  move "--- Job Details ---" to output-buffer
                                  perform outputLine
                                  move spaces to output-buffer
                                  string "Title: " delimited by size
                                         function trim(job-title trailing) delimited by size
                                         into output-buffer
                                  end-string
                                  perform outputLine

                                  move spaces to output-buffer
                                  string "Employer: " delimited by size
                                         function trim(job-employer trailing) delimited by size
                                         into output-buffer
                                  end-string
                                  perform outputLine

                                  move spaces to output-buffer
                                  string "Location: " delimited by size
                                         function trim(job-location trailing) delimited by size
                                         into output-buffer
                                  end-string
                                  perform outputLine

                                  move spaces to output-buffer
                                  string "Salary: " delimited by size
                                         function trim(job-salary trailing) delimited by size
                                         into output-buffer
                                  end-string
                                  perform outputLine

                                  move spaces to output-buffer
                                  string "Description: " delimited by size
                                         function trim(job-description trailing) delimited by size
                                         into output-buffer
                                  end-string
                                  perform outputLine

                                  *> Offer to apply
                                  move "[1] Apply to this job" to output-buffer
                                  perform outputLine
                                  move "[q] Back" to output-buffer
                                  perform outputLine
                                  move "> " to input-prompt
                                  perform readInputLine
                                  evaluate true
                                      when input-buffer = '1'
                                          *> Prepare buffers for application
                                          move function trim(current-user trailing) to application-username
                                          move function trim(job-key trailing) to application-job-key
                                          move function trim(job-title trailing) to application-job-title
                                          move function trim(job-employer trailing) to application-employer
                                          move function trim(job-location trailing) to application-location
                                          perform createApplication
                                      when input-buffer = '0' or input-buffer = 'q'
                                          *> go back to listings
                                          continue
                                      when other
                                          continue
                                  end-evaluate
                  end-read
              else
                  move "Unable to open job database." to output-buffer
                  perform outputLine
              end-if
              close job-database
              exit.

*>*******************************************************************
*> Create an application record for the current user for a job
*> Relies on application-* fields populated: application-username, application-job-key, application-job-title, application-employer, application-location
*>*******************************************************************
       createApplication.
              open i-o application-database
              if application-database-status = "35"
                  open output application-database
                  close application-database
                  open i-o application-database
              end-if

              if application-ok
                  *> Build key as username|jobkey
                  move spaces to application-key
                  string function trim(application-username trailing) delimited by size
                         '|' delimited by size
                         function trim(application-job-key trailing) delimited by size
                         into application-key
                  end-string

                  *> Fill record fields
                  move function trim(application-key trailing) to application-key
                  move function trim(application-username trailing) to application-username
                  move function trim(application-job-key trailing) to application-job-key
                  move function trim(application-job-title trailing) to application-job-title
                  move function trim(application-employer trailing) to application-employer
                  move function trim(application-location trailing) to application-location
                  move application-id-seq to application-id

                  write application-record

                  if application-ok
                      move "Application submitted successfully!" to output-buffer
                      perform outputLine
                      add 1 to application-id-seq
                  else
                      move "Failed to submit application." to output-buffer
                      perform outputLine
                  end-if
              else
                  move "Unable to open/create application database." to output-buffer
                  perform outputLine
              end-if
              close application-database
              exit.

*>*******************************************************************
*> View applications submitted by current user
*>*******************************************************************
       viewMyApplications.
              move "--- Your Job Applications ---" to output-buffer
              perform outputLine
              
              move spaces to output-buffer
              string "Application Summary for " delimited by size
                     function trim(current-user trailing) delimited by size
                     into output-buffer
              end-string
              perform outputLine
              
              move "------------------------------" to output-buffer
              perform outputLine
              
              move 0 to application-counter

              open input application-database
              if application-ok
                  perform until application-database-status not = "00"
                      read application-database next record
                          at end
                              exit perform
                          not at end
                              if function trim(application-username trailing) = function trim(current-user trailing)
                                  add 1 to application-counter
                                  move "Job Title: " to output-buffer
                                  string
                                      function trim(output-buffer trailing) delimited by size
                                      function trim(application-job-title trailing) delimited by size
                                  into output-buffer
                                  end-string
                                  perform outputLine
                                  
                                  move "Employer: " to output-buffer
                                  string
                                      function trim(output-buffer trailing) delimited by size
                                      function trim(application-employer trailing) delimited by size
                                  into output-buffer
                                  end-string
                                  perform outputLine
                                  
                                  move "Location: " to output-buffer
                                  string
                                      function trim(output-buffer trailing) delimited by size
                                      function trim(application-location trailing) delimited by size
                                  into output-buffer
                                  end-string
                                  perform outputLine
                                  
                                  move "---" to output-buffer
                                  perform outputLine
                              end-if
                      end-read
                  end-perform
              else
                  move "No applications found or database unavailable." to output-buffer
                  perform outputLine
              end-if
              close application-database

              move "------------------------------" to output-buffer
              perform outputLine
              
              move spaces to output-buffer
              string "Total Applications: " delimited by size
                     application-counter delimited by size
                     into output-buffer
              end-string
              perform outputLine
              
              move "------------------------------" to output-buffer
              perform outputLine
              
              if application-counter = 0
                  move "You have not submitted any applications yet." to output-buffer
                  perform outputLine
              end-if
              exit.

*>*******************************************************************
*> Messaging Menu
*>*******************************************************************
       messaging-menu.
              perform until not valid-read
                  move messages-title to output-buffer
                  perform outputLine
                  move messages-menu-1 to output-buffer
                  perform outputLine
                  move messages-menu-2 to output-buffer
                  perform outputLine
                  move messages-back to output-buffer
                  perform outputLine
                  move choice-prompt to input-prompt
                  perform readInputLine
                  move input-buffer(1:1) to menu-choice

                  evaluate true
                      when menu-choice = '1'
                          perform sendNewMessage
                      when menu-choice = '2'
                          perform viewMyMessages
                      when menu-choice = 'q' or not valid-read
                          exit perform
                      when other
                          move "Invalid choice. Please try again." to output-buffer
                          perform outputLine
                  end-evaluate
              end-perform
              exit.

*>*******************************************************************
*> Send a New Message
*> Validates that recipient is a connection before sending
*>*******************************************************************
       sendNewMessage.
              *> Prompt for recipient username
              move message-recipient-prompt to output-buffer
              perform outputLine
              perform readInputLine
              move function trim(input-buffer trailing) to message-recipient-buffer

              *> Validate recipient exists
              move message-recipient-buffer to buffer-acct-username
              perform findAcct
              if not acct-found
                  move message-user-not-found to output-buffer
                  perform outputLine
                  exit paragraph
              end-if

              *> Validate that recipient is a connection
              perform checkConnection
              if not-connected
                  move message-not-connected to output-buffer
                  perform outputLine
                  exit paragraph
              end-if

              *> Prompt for message content
              move message-content-prompt to output-buffer
              perform outputLine
              perform readInputLine
              move function trim(input-buffer trailing) to message-content-buffer

              *> Save message to database
              move current-user to message-sender-buffer
              perform saveMessage

              move message-sent-msg to output-buffer
              perform outputLine
              exit.

*>*******************************************************************
*> Check if two users are connected
*> Input: current-user (sender), message-recipient-buffer (recipient)
*> Output: Sets connection-status
*>*******************************************************************
              checkConnection.
              *> Default to not connected
              move "23" to connection-status
              
              *> Build connection keys for both directions
              open i-o connection-database
              if connection-ok
                  *> Check first direction (current-user|recipient)
                  move spaces to connection-key
                  string function trim(current-user trailing) delimited by size
                         '|' delimited by size
                         function trim(message-recipient-buffer trailing) delimited by size
                         into connection-key
                  end-string
                  
                  read connection-database
                      key is connection-key
                      invalid key
                          *> Not connected in first direction, check reverse
                          move spaces to connection-key
                          string function trim(message-recipient-buffer trailing) delimited by size
                                 '|' delimited by size
                                 function trim(current-user trailing) delimited by size
                                 into connection-key
                          end-string
                          
                          read connection-database
                              key is connection-key
                              invalid key
                                  *> Not connected in either direction
                                  move "23" to connection-status
                              not invalid key
                                  *> Connected in reverse direction
                                  move "00" to connection-status
                          end-read
                      not invalid key
                          *> Connected in forward direction
                          move "00" to connection-status
                  end-read
              end-if
              close connection-database
              exit.

*>*******************************************************************
*> Save Message to Database
*> Input: message-sender-buffer, message-recipient-buffer, message-content-buffer
*>*******************************************************************
       saveMessage.
              open i-o message-database
              if message-database-status = "35"
                  open output message-database
                  close message-database
                  open i-o message-database
              end-if

              if message-ok
                  *> Build unique message key using sender, recipient and sequence
                  move spaces to message-key
                  string function trim(message-sender-buffer trailing) delimited by size
                         '|' delimited by size
                         function trim(message-recipient-buffer trailing) delimited by size
                         '|' delimited by size
                         message-id-seq delimited by size
                         into message-key
                  end-string

                  *> Set message fields
                  move function trim(message-sender-buffer trailing) to message-sender
                  move function trim(message-recipient-buffer trailing) to message-recipient
                  move function trim(message-content-buffer trailing) to message-content
                  
                  *> Generate timestamp (simple format: using sequence number for now)
                  move spaces to message-timestamp
                  string "MSG-" delimited by size
                         message-id-seq delimited by size
                         into message-timestamp
                  end-string
                  
                  move 'N' to message-read-flag

                  *> Write message record
                  write message-record

                  if message-ok
                      add 1 to message-id-seq
                  else
                      move "Error saving message." to output-buffer
                      perform outputLine
                  end-if
              else
                  move "Unable to open message database." to output-buffer
                  perform outputLine
              end-if
              close message-database
              exit.

*>*******************************************************************
*> View My Messages (Under Construction)
*>*******************************************************************
       viewMyMessages.
              move view-messages-uc to output-buffer
              perform outputLine
              exit.
