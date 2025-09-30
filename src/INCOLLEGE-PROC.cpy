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
           move post-login-5 to output-buffer
           perform outputLine
           move logout to output-buffer
           perform outputLine
           move choice-prompt to output-buffer
           perform outputLine

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
