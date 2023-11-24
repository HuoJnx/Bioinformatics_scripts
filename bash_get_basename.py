#!/usr/bin/env python3

## description
#### It just for getting the basename in bash envs, because the GNU dirname can't support multiple file and should used
#### with parallel

import sys
import os

for line in sys.stdin:
    line = line.strip()  # Remove leading/trailing white space
    print(os.path.basename(line))
