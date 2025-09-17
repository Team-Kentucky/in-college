# InCollege Testing Guide

## Quick Start
To run the full suite of tests, run `./run-test.sh`. Each test will be compared against its pass fail condition. Each test will display a P (pass) or a F (fail). All test output will be stored in bin/test-output

### Build and Run
```bash
make clean && make
./run-tests.sh
```

### Manual Testing
You can also run tests manually. To do so, ensure that your input file is located in `bin/input.txt` and your account database is located `bin/acct-database.dat`. Once you have done so, you can simply run `./inCollege`:

### Manual Testing Example
```bash
cp input-files/account-creation-test.txt bin/input.txt && cp input-files/account-creation-test-database.dat bin/acct-databse.dat
cd bin && ./inCollege
```

### Clean Up
```bash
rm -f bin/input.txt bin/output.txt bin/acct-database.dat
```

## Test Files
- `account-creation-test.txt` - Tests account creation
- `account-limit-test.txt` - Tests the account limit
- `account-persistance-test.txt` - Tests account persistance
- `incorrect-credentials-test.txt` - Tests logging in with incorrect credentials
- `job-search-test.txt` - Tests the job search menu button
- `password-validation-test.txt` - Tests the password requirements on account creation
- `profile-all-optional-test.txt` - Tests a creating a full profile
- `profile-blank-required-test.txt` - Tests attempting to leave required profile fields blank
- `profile-editing-test.txt` - Tests profile editing
- `profile-grad-date-test.txt` - Tests entering an invalid graduation date
- `profile-persistance-test.txt` - Tests profile persistance
- `profile-required-test.txt` - Tests only entering required profile fields
- `profile-view-empty-test.txt` - Tests attempting to view a profile that hasn't been created yet
- `search-user-test.txt` - Tests the search user button
- `skill-1-test.txt` - Tests the skill 1
- `skill-2-test.txt` - Tests the skill 2
- `skill-3-test.txt` - Tests the skill 3
- `skill-4-test.txt` - Tests the skill 4
- `skill-5-test.txt` - Tests the skill 5