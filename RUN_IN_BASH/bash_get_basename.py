#!/usr/bin/env python3

## description
#### It just for getting the basename in bash envs, because the GNU dirname can't support multiple file and should used
#### with parallel


import sys
import os


if not sys.stdin.isatty(): ## check if input from stdin
    for line in sys.stdin:
        line = line.strip()  # Remove leading/trailing white space
        print(os.path.basename(line))
else:
    sys.exit("Error: No input provided through stdin. Please provide input.")