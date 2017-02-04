# Yamaha Rm1x Drumkits

A [pdf file](https://raw.githubusercontent.com/karamellpelle/rm1x-template/master/YamahaRm1x.pdf) with all mappings can be generated with the command `./makebook.sh`.

Svg's for single kits can also be generated, but they are in Inkscape format and uses the `flowRoot` object. [This makes them not very compatible with other viewers.](http://stackoverflow.com/a/19391378/753850)
But I suppose it is possible to "flatten" the generated svg files from command line by Inkscape. And you can also export them to other formats by command line.

## Update!

After making this book, I found bugs in the list book: several kits was off by one seminote. They have been corrected here. The affected kits was
BDKit, HHCyKit, SDKit, TomKit, SFXKit1 and SFXKit2. 



## rm1x-template

### Building

This project depends on Haskell, Inkscape, LaTeX and Unix (and packages for them). 
To build the program `rm1x-template`:

    $ runhaskell Setup configure
    $ runhaskell Setup build

### Usage

For help: `rm1x-template --help`.

Create 1 svg file from kit:

    $ rm1x-template --svg kit.txt [--output file|folder]

Divide kit map to 3 svg files (used extensively by book):

    $ rm1x-template --svg-pages kit.txt [--output folder]

Create pdf file from kits:

    $ rm1x-template --book kit1.txt ... kitN.txt [--output file|folder]

The `--force` flag can be used to overwrite files.


## TODO

The program should ideally be profiled and optimized, since it uses very much CPU and memory when creating a book
from several kits :) Some strictness in evaluation would probably fix this.
