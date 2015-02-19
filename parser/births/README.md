# Birth Parser

## Building

I'm hoping it's enough to run, but you'll probably need GHC.

    make all

## Usage

    pdftotext -enc UTF-8 -table path/to/report/ugly.pdf
    ./dist/build/Births/births path/to/report/ugly.txt > output.csv

