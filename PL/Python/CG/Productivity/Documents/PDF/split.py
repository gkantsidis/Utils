"""
Split a PDF file into multiple files
"""

import argparse
import os
import sys
from typing import List, NamedTuple
from csv import DictReader
from pathlib import Path
from PyPDF3 import PdfFileWriter, PdfFileReader


Chapter = NamedTuple('Chapter',
                     [
                       ('name', str),
                       ('start', int),
                       ('stop', int)
                     ])


def _parse_splits_file(filename: str, newline='') -> List[Chapter]:
    assert os.path.exists(filename)

    output: List[Chapter] = []
    with open(filename, newline='') as file:
        reader = DictReader(file)
        for row in reader:
            name = row['name']
            start = int(row['start'])
            stop = int(row['end'])
            entry = Chapter(name, start, stop)
            output.append(entry)

    return output


def split(filename: str, chapters: List[Chapter], directory: str) -> None:
    assert os.path.exists(filename)

    for chapter in chapters:
        print(f'Writing to {chapter.name} pages {chapter.start} to {chapter.stop}')

        # The file should have been opened before the previous loop,
        # however, there is a bug in the library, and this is the only way to
        # get around it.
        with open(filename, 'rb') as input_stream:
            input_pdf = PdfFileReader(input_stream)

            output = PdfFileWriter()
            for page_number in range(chapter.start-1, chapter.stop):
                page = input_pdf.getPage(page_number)
                output.addPage(page)

            output_filename = os.path.join(directory, chapter.name)
            with open(output_filename, "wb") as output_stream:
                output.write(output_stream)
                output_stream.flush()


if __name__ == '__main__':
    _parser = argparse.ArgumentParser(description='Split pages of a PDF file')
    _parser.add_argument('filename', type=str, help='File to split')
    _parser.add_argument('chapters', type=str, help='List of splits to create (in CSV format)')

    args = _parser.parse_args()

    if os.path.exists(args.filename) is False:
        sys.stderr.write(f'Input file {args.filename} does not exist')
        exit(-1)

    if os.path.exists(args.chapters) is False:
        sys.stderr.write(f'Chapter file {args.chapters} already exists')
        exit(-1)

    _directory = Path(args.filename).parent.absolute()
    _chapters = _parse_splits_file(args.chapters)
    split(args.filename, _chapters, _directory)
