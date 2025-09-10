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
- Docker (Reccomended)
- GnuCOBOL (Alternative) (macOS: `brew install gnu-cobol`)

### Build
```bash
make
```

Or if you would prefer to run them yourself:
```bash
mkdir -p bin
cobc -Isrc/ -x -o bin/inCollege src/inCollege.cob
```

### Run
If in Docker/Linux, you can run using test.sh
It will automatically pull the correct input files and run the full suite of tests (which are located in input-files/). It will also automatically pipe the command line output into bin/cli-output.txt for easier comparison and to automatically use in a diff against output.txt
```bash
./test.sh
```

Alternatively you can copy the input file into bin (or wherever the binary is), rename it to `input.txt`, cd into bin, and then run the executable.
Notes: You must ensure that your input file is named `input.txt`. It also must be in the directory you are currently in (eg if you run the program while in the main project folder with `./bin/inCollege`, ensure that `input.txt` is also in the main project folder).
Example commands:
```bash
make
cp <your input file> bin/input.txt
cd bin
./inCollege
```

## Input/Output Files

### Input File: `input.txt`
The program reads all user inputs from this file. input.txt must be located in the directory you are in when running the binary. Each line represents a single input:
- Menu choices
- Usernames and passwords
- Navigation choices within logged-in menus

### Output File: `output.txt`
All program output is mirrored to this file, identical to console output.

### Accounts File: `acct-database.dat`
User accounts are persisted in this indexed file (created automatically).

## Sample Test

Create `input.txt` with this content to test the complete flow:
```
1
u-demo
Passw0rd!
0
u-demo
Passw0rd!
3
1
q
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
1
username
Password1!
```

### Test Login
```
0
username
Password1!
```

### Test Invalid Password
```
1
user
short
```

### Test Account Limit (after 5 accounts exist)
```
1
user6
Password1!
```

## Reset State
To start fresh testing:
```bash
rm acct-database.dat
rm output.txt
```

## Expected Output Format
The program displays an ASCII-art menu and all output is mirrored to `output.txt`. Console and file outputs are identical.