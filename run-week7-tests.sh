#!/bin/bash

# Week 7 Test Execution Script
# This script runs all Week 7 test cases and generates output files

echo "Starting Week 7 Test Execution..."
echo "================================="

# Test 1: Browse Empty Jobs
echo "Test 1: Browse Empty Jobs"
cp input-files/browse-empty-jobs-test.txt input.txt
./bin/inCollege > output-files/browse-empty-jobs-output.txt 2>&1
echo "✓ Completed: browse-empty-jobs-output.txt"

# Test 2: Apply to Job
echo "Test 2: Apply to Job"
cp input-files/apply-to-job-test.txt input.txt
./bin/inCollege > output-files/apply-to-job-output.txt 2>&1
echo "✓ Completed: apply-to-job-output.txt"

# Test 3: View Applications
echo "Test 3: View Applications"
cp input-files/view-applications-test.txt input.txt
./bin/inCollege > output-files/view-applications-output.txt 2>&1
echo "✓ Completed: view-applications-output.txt"

# Test 4: View Empty Applications
echo "Test 4: View Empty Applications"
cp input-files/view-empty-applications-test.txt input.txt
./bin/inCollege > output-files/view-empty-applications-output.txt 2>&1
echo "✓ Completed: view-empty-applications-output.txt"

# Test 5: Duplicate Application Prevention
echo "Test 5: Duplicate Application Prevention"
cp input-files/duplicate-application-test.txt input.txt
./bin/inCollege > output-files/duplicate-application-output.txt 2>&1
echo "✓ Completed: duplicate-application-output.txt"

echo ""
echo "All Week 7 tests completed!"
echo "Check output-files/ directory for results."
echo "================================="

