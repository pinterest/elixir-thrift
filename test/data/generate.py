#!/usr/bin/env python
"""Generates static test data.

See `README.md` for instructions on running this script.
"""
from glob import iglob
import os
import re
import shutil
import subprocess
import sys
import test_data

def mkdir(dir):
    try:
        os.mkdir(dir)
    except FileExistsError:
        pass


def find_thrift_definitions():
    thrift_definitions = []
    for filename in iglob('../**/*.exs', recursive=True):
        file_contents = ''
        with open(filename, 'r') as f:
            file_contents = f.read()
        results = re.findall(r'@thrift_file name: "(.+?)", contents: """(.*?)"""', file_contents, re.DOTALL)
        thrift_definitions.extend(results)
    return thrift_definitions


def write_thrift_files(thrift_definitions):
    mkdir('thrift')
    thrift_files = []
    for filename, definition in thrift_definitions:
        full_path = os.path.join('thrift', filename)
        with open(full_path, 'w') as f:
            f.write(definition)
            thrift_files.append(full_path)
    return thrift_files


def generate_python_files(thrift_files):
    mkdir('generated')
    for thrift_file in thrift_files:
        command = 'thrift -I thrift --gen py --out generated'.split(' ') + [thrift_file]
        subprocess.run(command)
    sys.path.append('generated')


def cleanup():
    shutil.rmtree('thrift', ignore_errors=True)
    shutil.rmtree('generated', ignore_errors=True)


def main():
    try:
        thrift_definitions = find_thrift_definitions()
        thrift_files = write_thrift_files(thrift_definitions)
        generate_python_files(thrift_files)
        test_data.write()
    finally:
        cleanup()


if __name__ == '__main__':
    main()
