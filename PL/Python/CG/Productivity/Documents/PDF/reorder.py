"""
Reorder pages in a pdf file
"""

import argparse
import os
import sys
from typing import List
from PyPDF3 import PdfFileWriter, PdfFileReader


def _make_sequence(n: int) -> List[int]:
    assert n % 2 == 0
    pages = range(0, n)
    half = len(pages) // 2
    left = list(pages[:half])
    right = list(pages[half:])
    right.reverse()
    correct = list(range(0, n))
    correct[::2] = left
    correct[1::2] = right
    return correct


def reorder(input_filename: str, output_filename: str) -> None:
    assert os.path.exists(input_filename)
    assert os.path.exists(output_filename) is False

    input_stream = open(input_filename, 'rb')
    output = PdfFileWriter()
    input_pdf = PdfFileReader(input_stream)

    pages = input_pdf.getNumPages()
    order = _make_sequence(pages)

    for page_number in order:
        page = input_pdf.getPage(page_number)
        output.addPage(page)

    output_stream = open(output_filename, "wb")
    output.write(output_stream)
    input_stream.close()
    output_stream.close()


if __name__ == '__main__':
    _parser = argparse.ArgumentParser(description='Reorder pages in a PDF file')
    _parser.add_argument('filename', type=str, help='File to reorder')
    _parser.add_argument('output', type=str, help='Output file')

    args = _parser.parse_args()

    if os.path.exists(args.filename) is False:
        sys.stderr.write(f'Input file {args.filename} does not exist')
        exit(-1)

    if os.path.exists(args.output) and args.filename != args.output:
        sys.stderr.write(f'Output file {args.output} already exists')
        exit(-1)

    _input_filename = args.filename
    _output_filename = args.output

    reorder(_input_filename, _output_filename)
