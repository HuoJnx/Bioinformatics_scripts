#!/usr/bin/env python3


"""
get_n_rows.py: Extract specific lines from stdin.

Modes:
1. Single Line Mode: Extract a single line.
   Usage: cat text_file | ./get_n_rows.py <line_num>

2. Range Mode: Extract a range of continuous lines.
   Usage: cat text_file | ./get_n_rows.py <start_line> <end_line>

3. Comma-separated Mode: Extract non-continuous lines specified by a comma-separated list.
   Usage: cat text_file | ./get_n_rows.py <line_num1,line_num2,...>

Example:
cat text_file | ./get_n_rows.py 5  # Extracts the 5th line.
cat text_file | ./get_n_rows.py 3 7  # Extracts lines from 3rd to 7th.
cat text_file | ./get_n_rows.py 1,5,9  # Extracts the 1st, 5th, and 9th lines.
"""



import sys

class LineExtractor:
    def __init__(self, lines):
        self.lines = [line.strip() for line in lines]
        self.n_lines = len(self.lines)

    def mode_single_line(self, line_num):
        """Extract a single line based on provided line number."""
        self._validate_line_num(line_num)
        print(self.lines[line_num - 1])

    def mode_range(self, start_line, end_line):
        """Extract a range of lines from start_line to end_line."""
        self._validate_line_num(start_line)
        self._validate_line_num(end_line)
        for i in range(start_line - 1, end_line):
            print(self.lines[i])

    def mode_comma_separated(self, line_nums_str):
        """Extract non-continuous lines based on a comma-separated list."""
        line_nums = [int(num) for num in line_nums_str.split(",")]
        for line_num in line_nums:
            self._validate_line_num(line_num)
        for line_num in line_nums:
            print(self.lines[line_num - 1])

    def _validate_line_num(self, line_num):
        if line_num > self.n_lines:
            raise Exception(f"Error: Line number {line_num} is out of range. The input has only {self.n_lines} lines.")

def main():
    extractor = LineExtractor(sys.stdin)

    if len(sys.argv) == 2:
        arg = sys.argv[1]
        if "," in arg:
            extractor.mode_comma_separated(arg)
        else:
            extractor.mode_single_line(int(arg))
    elif len(sys.argv) == 3:
        start_line = int(sys.argv[1])
        end_line = int(sys.argv[2])
        extractor.mode_range(start_line, end_line)
    else:
        raise Exception("Invalid number of arguments provided.")

if __name__ == "__main__":
    main()
