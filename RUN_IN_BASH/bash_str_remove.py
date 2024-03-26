#!/usr/bin/env python3

import re
import sys

def remove_string(line, pattern):
    # Use regular expression to remove the pattern
    result = re.sub(pattern, '', line, count=1)
    return result

# Check if stdin is not empty
if not sys.stdin.isatty():
    # Get the pattern from the command line argument
    if len(sys.argv) < 2:
        sys.exit("Error: Please provide a pattern as a command-line argument.")

    pattern = sys.argv[1]  # The pattern to remove is the first argument

    # Read from stdin line by line
    for line in sys.stdin:
        line = line.strip()  # Remove leading/trailing white space
        print(remove_string(line, pattern))
else:
    sys.exit("Error: No input provided through stdin. Please provide input.")
