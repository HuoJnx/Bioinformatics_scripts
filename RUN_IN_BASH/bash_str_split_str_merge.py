#!/usr/bin/env python3
import sys

def str_split_str_merge(line, sep, keep_range):
    # Split the line by the separator
    parts = line.split(sep)

    # Keep only the elements in the specified range
    kept_parts = parts[keep_range[0]-1 : keep_range[1]]

    # Merge the kept parts back together
    merged = sep.join(kept_parts)

    return merged

# Get the separator and range from command line arguments
sep = sys.argv[1]  # The separator is the first argument
start = int(sys.argv[2])  # The start of the range is the second argument
end = int(sys.argv[3])  # The end of the range is the third argument
keep_range = (start, end)

# Read from stdin line by line
for line in sys.stdin:
    line = line.strip()  # Remove leading/trailing white space
    print(str_split_str_merge(line, sep, keep_range))

