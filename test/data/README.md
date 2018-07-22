# Static test data

This directory contains serialized thrift binary data that the tests use for
validation.

## Generating test data

To generate the test data, you will need to have Python 3.5+ and `thrift`
installed. You should create a virtual environment and install the Python
thrift library, after which you can run the `generate.py` script. Here is a
full example, which you may need to alter depending on your setup:

    python3 -m venv .venv
    source .venv/bin/activate
    pip install thrift
    ./generate.py

## Adding new test data

The file `test_data.py` contains a `write()` method, which creates thrift
structures and serializes them to disk. Any new serialization/deserialization
test cases should have corresponding entries added in here.
