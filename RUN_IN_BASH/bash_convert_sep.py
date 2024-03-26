#!/usr/bin/env python3
"""
bash_convert_separator.py

This script reads a table from standard input and converts its separator.

Usage:
    cat input_table.txt | ./bash_convert_separator.py [from_sep] [to_sep]

Options:
    from_sep : The original separator (supports regular expressions)
    to_sep   : The new separator

Example:
    cat input_table.txt | ./bash_convert_separator.py " +" "\t"
"""

import sys
import pandas as pd

def convert_special_chars(char):
    special_chars = {
        "\\t": "\t",
        "\\n": "\n",
        "\\r": "\r"
        # Add more special characters here if needed
    }
    return special_chars.get(char, char)  # The second argument is the default output when the dictionary doesn't have this key.

def convert_separator(from_sep, to_sep):
    # Convert string representations of special characters to the actual character
    from_sep = convert_special_chars(from_sep)
    to_sep = convert_special_chars(to_sep)

    # Check if to_sep is a single character
    if len(to_sep) != 1:
        print("Error: The 'to_sep' must be a single character.")
        sys.exit(1)

    # Read the table into a pandas DataFrame
    df = pd.read_csv(sys.stdin, sep=from_sep, header=None, engine="python", dtype=str)

    # Convert the DataFrame back to a string with the new separator and print to stdout
    df.to_csv(sys.stdout, sep=to_sep, index=False, header=False)

def main():
    # Check if stdin is not empty
    if not sys.stdin.isatty():
        # Check for help flag or no arguments
        if len(sys.argv) == 1 or sys.argv[1] in ['-h', '--help']:
            print(__doc__)
            return

        # Get the separators from the command line arguments
        from_sep = sys.argv[1]
        to_sep = sys.argv[2]
        # Convert the separator
        convert_separator(from_sep, to_sep)
    else:
        sys.exit("Error: No input provided through stdin. Please provide input.")

if __name__ == "__main__":
    main()
