# Yamaha Rm1x Drumkits

A [pdf file](https://raw.githubusercontent.com/karamellpelle/rm1x-template/master/YamahaRm1x.pdf) with all mappings can be generated with the following command:

    ./rm1x-template --book kits/AnlgKit1.txt kits/AnlgKit2.txt kits/RhBoxKit.txt kits/SynthKit.txt kits/SEKit.txt kits/PsychKit.txt kits/AcidKit.txt kits/TeknoKit.txt kits/AmbntKit.txt kits/HardKit.txt kits/HouseKit.txt kits/BreakKit.txt kits/JunglKit.txt kits/DnBKit.txt kits/BigKit.txt kits/HipHpKit.txt kits/AcoKit.txt kits/JazzKit.txt kits/BrushKit.txt kits/PercsKit.txt kits/BDKit.txt kits/HHCyKit.txt kits/SDKit.txt kits/TomKit.txt kits/SFXKit1.txt kits/SFXKit2.txt kits/Notes.txt

Svg's for single kits can also be generated, but they are in Inkscape format and uses the `flowRoot` object. [This makes them not very compatible with other viewers.](http://stackoverflow.com/a/19391378/753850)
But I suppose it is possible to "flatten" the generated svg files from command line by Inkscape. And you can also export them to other formats by command line.

## `rm1x-template`

### Building

This project depends on Haskell, Inkscape, LaTeX and Unix (and packages for them). 
To build the program `rm1x-template`:

    $ runhaskell Setup configure
    $ runhaskell Setup build

### Usage

For help: `rm1x-template --help`.

Create 1 svg file from kit:

    $ rm1x-template --svg kit.txt [--output file|folder]

Create 3 svg files from kit, each svg in A4-format:

    $ rm1x-template --svg-pages kit.txt [--output folder]

Create pdf file from kits:

    $ rm1x-template --book kit1.txt ... kitN.txt [--output file|folder]

The `--force` flag can be used to overwrite files.


## TODO

The program should ideally be profiled and optimized, since it uses very much CPU and memory when creating a book
from several kits :) Some strictness in evaluation would probably fix this.
