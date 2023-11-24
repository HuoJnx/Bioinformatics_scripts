#!/bin/bash

## description
#### Just for counting GFF, you just need to provide the filepath and it will give you the answer.

# Count the number of lines in the file, excluding any that start with '#' or just empty line
num_records=$(grep -Pv '^#|^$' "$1" | wc -l)

echo "$num_records"