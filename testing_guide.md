# InCollege Testing Guide

## Overview
This guide provides comprehensive testing instructions for the InCollege COBOL project, specifically for the `feat/commit3-login-navigation` branch which implements the complete Week 3 requirements.

## Prerequisites

### Software Requirements
- **GnuCOBOL**: Install from [gnucobol.sourceforge.io](https://gnucobol.sourceforge.io/)
  - macOS: `brew install gnu-cobol`
  - Ubuntu/Debian: `sudo apt-get install open-cobol`
  - Windows: Download from official site
  - Docker: Follow the slides if you want to run it in any environment

### Git Setup
- Git installed and configured
- Access to the repository: `https://github.com/Team-Kentucky/in-college.git`

## Getting Started

### 1. Clone and Setup
```bash
# Clone the repository
git clone https://github.com/Team-Kentucky/in-college.git
cd in-college

# Switch to Commit 3 branch
git checkout feat/commit3-login-navigation

# Build the program
mkdir -p bin
cobc -x -o bin/inCollege src/inCollege.cob
```

### 2. Reset State (Fresh Start)
```bash
# Remove existing data files
rm -f accounts.dat InCollege-Output.txt
```

### Quick Test Input: `InCollege-QuickTest.txt`
```text
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

## Test Execution

### Method 1: Manual Testing
```bash
# Copy test input into the InCollege-Input.txt file
cp InCollege-Test.txt InCollege-Input.txt

# Run the program
./bin/inCollege

# Verify output
cat InCollege-Output.txt
```

## Test Coverage

### Positive Test Cases ✅
1. **Account Creation (5 accounts)**
   - Valid usernames and passwords
   - Expected: "Account created successfully."

2. **Successful Login (5 accounts)**
   - Login with each created account
   - Expected: "You have successfully logged in" + "Welcome, [username]"

3. **Post-Login Navigation**
   - Job Search, Find Someone, Learn Skills
   - Expected: Appropriate "under construction" messages

4. **Skills Menu Navigation**
   - All 5 skills + Go Back functionality
   - Expected: "This skill is under construction." for each skill

### Negative Test Cases ❌
1. **Account Limit (6th attempt)**
   - Try to create 6th account after 5 exist
   - Expected: "All permitted accounts have been created, please come back later"

2. **Duplicate Username**
   - Try to create account with existing username
   - Expected: "Username already exists. Please try again."

3. **Invalid Passwords**
   - Too short, no digit, no capital, no special character
   - Expected: "Invalid password, please try again"

4. **Failed Login Attempts**
   - Wrong password
   - Expected: "Incorrect username/password, please try again"

### Edge Cases 🔍
1. **Password Boundaries**
   - Exactly 8 characters: `Passw0rd!`
   - Exactly 12 characters: `Password123!`
   - Missing requirements: `Short1!`, `NoDigit!`, `lowercase1!`, `NoSpecial1`

2. **Menu Navigation**
   - Invalid choices in main menu
   - Invalid choices in skills menu
   - Go Back functionality

## Expected Output Verification

### Required Messages in `InCollege-Output.txt`:
1. **Welcome and Menu** (repeated for each interaction)
   - ASCII-art menu with borders
   - "Log In", "Create New Account", "Enter your choice:"

2. **Account Creation Messages**
   - "Please enter your username:"
   - "Please enter your password:"
   - "Account created successfully." (5 times)
   - "All permitted accounts have been created, please come back later" (1 time)
   - "Username already exists. Please try again." (1 time)
   - "Invalid password, please try again" (4 times)

3. **Login Messages**
   - "You have successfully logged in" (5 times)
   - "Welcome, [username]" (5 times)
   - "Incorrect username/password, please try again" (1 time)

4. **Post-Login Navigation**
   - "Job search/internship is under construction."
   - "Find someone you know is under construction."
   - Skills menu with 5 skills + "Go Back"
   - "This skill is under construction." (5 times)

5. **End Marker**
   - "--- END_OF_PROGRAM_EXECUTION ---"

## Verification Commands

### Check Output Files
```bash
# View the complete output
cat InCollege-Output.txt

# Count specific messages
grep -c "Account created successfully" InCollege-Output.txt
grep -c "You have successfully logged in" InCollege-Output.txt
grep -c "under construction" InCollege-Output.txt
```

### Check Accounts Persistence
```bash
# View saved accounts
cat accounts.dat

# Count accounts (should be 5)
wc -l accounts.dat
```

## Troubleshooting

### Common Issues
1. **"Invalid choice" messages**: Ensure input file has correct format with no blank lines
2. **"Username already exists"**: Account was created in previous test; reset with `rm accounts.dat`
3. **"All permitted accounts have been created"**: Hit 5-account limit; reset state
4. **Program hangs**: Check input file has enough lines for all menu choices

### Reset Commands
```bash
# Complete reset
rm -f accounts.dat InCollege-Output.txt InCollege-Input.txt

# Rebuild
cobc -x -o bin/inCollege src/inCollege.cob
```

## Branch Information
- **Branch**: `feat/commit3-login-navigation`
- **Commit**: `6797660`
- **Status**: Ready for testing
- **Features**: Complete login system + post-login navigation + skills menu

## Success Criteria
✅ All positive test cases pass  
✅ All negative test cases handled correctly  
✅ All edge cases work as expected  
✅ Console output matches file output exactly  
✅ 5 accounts created and persisted  
✅ All required messages appear in correct order  
✅ Program terminates with end marker  

## Reporting Issues
When reporting bugs, include:
1. Input file used
2. Expected vs actual output
3. Console output vs file output differences
4. Steps to reproduce
5. System information (OS, GnuCOBOL version)

---