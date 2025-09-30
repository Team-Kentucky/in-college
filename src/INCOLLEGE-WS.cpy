
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
