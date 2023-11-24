#!/usr/bin/env python3

"""
get_n_cols.py: Extract specific columns from stdin using pandas.

Modes:
1. Single Column Mode: Extract a single column.
   Usage: cat text_file | ./get_n_cols.py <col_num> <separator>

2. Range Mode: Extract a range of continuous columns.
   Usage: cat text_file | ./get_n_cols.py <start_col> <end_col> <separator>

3. Comma-separated Mode: Extract non-continuous columns specified by a comma-separated list.
   Usage: cat text_file | ./get_n_cols.py <col_num1,col_num2,...> <separator>

Example:
cat text_file | ./get_n_cols.py 5 ','  # Extracts the 5th column using comma as a separator.
cat text_file | ./get_n_cols.py 3 7 ','  # Extracts columns from 3rd to 7th using comma as a separator.
cat text_file | ./get_n_cols.py 1,5,9 ','  # Extracts the 1st, 5th, and 9th columns using comma as a separator.
cat text_file | ./get_n_cols.py -2 ','  # Extracts the second last column.
"""

import sys
import pandas as pd

def adjust_col_num(col_num, total_cols):
    """Adjust column number to support negative indexing."""
    return col_num if col_num >= 0 else total_cols + col_num + 1

def mode_single_column(df, col_num):
    """Extract a single column based on provided column number."""
    col_num = adjust_col_num(col_num, df.shape[1])
    return df.iloc[:, col_num - 1]

def mode_range(df, start_col, end_col):
    """Extract a range of columns from start_col to end_col."""
    start_col = adjust_col_num(start_col, df.shape[1])
    end_col = adjust_col_num(end_col, df.shape[1])
    return df.iloc[:, start_col - 1:end_col]

def mode_comma_separated(df, col_nums_str):
    """Extract non-continuous columns based on a comma-separated list."""
    col_nums = [adjust_col_num(int(num), df.shape[1]) - 1 for num in col_nums_str.split(",")]
    return df.iloc[:, col_nums]

def main():
    if len(sys.argv) == 1 or sys.argv[1] in ['-h', '--help']:
        print(__doc__)
        return
    
    separator = sys.argv[-1]
    df = pd.read_csv(sys.stdin, sep=separator, header=None, engine='python', dtype=str)

    if len(sys.argv) == 3:
        arg = sys.argv[1]
        if "," in arg:
            result = mode_comma_separated(df, arg)
        else:
            result = mode_single_column(df, int(arg))
    elif len(sys.argv) == 4:
        start_col = int(sys.argv[1])
        end_col = int(sys.argv[2])
        result = mode_range(df, start_col, end_col)
    else:
        raise Exception("Invalid number of arguments provided.")

    result.to_csv(sys.stdout, sep="\t",index=False, header=False)

if __name__ == "__main__":
    main()
