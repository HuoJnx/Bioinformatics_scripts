#!/usr/bin/env python3

## description
#### It's just mimicing the str_remove function in R, because the str removal function is stupid in bash.

import re
import sys

def remove_string(line, pattern):
    # Use regular expression to remove the pattern
    result = re.sub(pattern, '', line, count=1)
    return result

# Get the pattern from the command line argument
pattern = sys.argv[1]  # The pattern to remove is the first argument

# Read from stdin line by line
for line in sys.stdin:
    line = line.strip()  # Remove leading/trailing white space
    print(remove_string(line, pattern))

