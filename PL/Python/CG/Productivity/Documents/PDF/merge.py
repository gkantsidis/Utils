"""
Merges a set of files
"""

__all__ = ('merge')

import argparse
import os
import pikepdf
import sys
from glob import glob
from pathlib import Path
from typing import List, NamedTuple, Optional


def _expand_files(filenames: List[str]) -> List[str]:
    result = []

    for filename in filenames:
        if os.path.isfile(filename):
            result = result + filename
        if os.path.isdir(filename):
            path = Path(filename)
            result = result + path.glob("*.*")
        else:
            result = result + glob(filename)

    return result


def merge(output: str, filenames: List[str], skip_front: Optional[int] = None):
    merged = pikepdf.Pdf.new()

    for filename in filenames:
        print(f'Adding {filename}\n')

        with pikepdf.open(filename) as source:
            if skip_front:
                merged.pages.extend(source.pages[skip_front:])
            else:
                merged.pages.extend(source.pages)

    merged.save(output)


if __name__ == '__main__':
    _parser = argparse.ArgumentParser(description='Merge PDF files into a single file')
    _parser.add_argument('-stripfront', type=int, help='Number of pages to strip from the beginning of each input file')
    _parser.add_argument('output', type=str, help='Target file')
    _parser.add_argument('input', nargs='*')

    args = _parser.parse_args()

    if not args.output:
        sys.stderr.write("Need to specify output file name")
        exit(-1)

    if os.path.exists(args.output) is True:
        sys.stderr.write(f'Output file {args.output} already exists')
        exit(-1)

    if not args.input:
        sys.stderr.write("Input list of files is empty")
        exit(-1)

    inputs = _expand_files(args.input)
    for filename in inputs:
        if os.path.exists(filename) is False:
            sys.stderr.write(f'Input file {args.input} does not exist')
            exit(-1)

    if args.stripfront:
        merge(args.output, inputs, args.stripfront)
    else:
        merge(args.output, inputs)
