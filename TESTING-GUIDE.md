# InCollege Testing Guide

## Quick Start

### Build and Run
```bash
make clean && make
cd bin && ./inCollege
```

### Test Profile Management
```bash
# Basic profile test
cp input-files/profile-test-basic.txt bin/input.txt && cd bin && ./inCollege

# Complete profile test  
cp input-files/profile-test-complete.txt bin/input.txt && cd bin && ./inCollege

# Validation test
cp input-files/profile-test-validation.txt bin/input.txt && cd bin && ./inCollege

# Profile editing test
cp input-files/profile-test-edit.txt bin/input.txt && cd bin && ./inCollege
```

### Clean Up
```bash
rm -f bin/input.txt bin/output.txt bin/acct-database.dat bin/acct-database.idx
```

## Test Files
- `profile-test-basic.txt` - Basic profile creation
- `profile-test-complete.txt` - Full profile with experience/education
- `profile-test-validation.txt` - Input validation testing
- `profile-test-edit.txt` - Profile editing functionality