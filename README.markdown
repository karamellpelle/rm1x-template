# Yamaha Rm1x Drumkit Keymaps

## Build

    $ runhaskell Setup configure
    $ runhaskell Setup build

## Usage

Create 1 svg file from kit:

    $ rm1x-template --svg kit.txt [--output file|folder]

Create 3 svg files from kit, each svg in A4-format:

    $ rm1x-template --svg-pages kit.txt [--output folder]

Create pdf file from kits:

    $ rm1x-template --book kit1.txt ... kitN.txt [--output file|folder]

The `--force` flag can be used to overwrite files.
