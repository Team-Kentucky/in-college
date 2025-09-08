## Overview
InCollege is a LinkedIn-like platform for college students, implemented in COBOL. The program provides user registration, login functionality, and basic navigation features.

## Features
- User account creation with password validation (8-12 chars, 1 uppercase, 1 digit, 1 special character)
- Account persistence using sequential file storage
- 5-account limit per system
- Login with unlimited attempts
- Post-login navigation: Job Search, Find Someone, Learn New Skills
- All features show "under construction" messages as specified

## How to Build and Run

### Prerequisites
- GnuCOBOL installed (macOS: `brew install gnu-cobol`)

### Build
```bash
mkdir -p bin
cobc -x -o bin/inCollege src/inCollege.cob
```

### Run
```bash
./bin/inCollege
```

## Input/Output Files

### Input File: `InCollege-Input.txt`
The program reads all user inputs from this file. Each line represents a single input:
- Menu choices: `1` (Login), `2` (Create Account)
- Usernames and passwords
- Navigation choices within logged-in menus

### Output File: `InCollege-Output.txt`
All program output is mirrored to this file, identical to console output.

### Accounts File: `accounts.dat`
User accounts are persisted in this line-sequential file (created automatically).

## Sample Test

Create `InCollege-Input.txt` with this content to test the complete flow:
```
2
u-demo
Passw0rd!
1
u-demo
Passw0rd!
3
1
6
```

This test will:
1. Create a new account (u-demo with Passw0rd!)
2. Login with the created account
3. Navigate to "Learn a new skill" menu
4. Select skill 1 (shows "under construction")
5. Go back to main menu
6. End program

## Testing Different Scenarios

### Test Account Creation
```
2
username
Password1!
```

### Test Login
```
1
username
Password1!
```

### Test Invalid Password
```
2
user
short
```

### Test Account Limit (after 5 accounts exist)
```
2
user6
Password1!
```

## Reset State
To start fresh testing:
```bash
rm accounts.dat
rm InCollege-Output.txt
```

## Expected Output Format
The program displays an ASCII-art menu and all output is mirrored to `InCollege-Output.txt`. Console and file outputs are identical.