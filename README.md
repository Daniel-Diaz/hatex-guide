# HaTeX User's Guide

_Welcome to the HaTeX User's Guide!_

A pdf version (created from the LaTeX output) can be downloaded from
[here](http://daniel-diaz.github.com/projects/hatex/hatex-guide.pdf).

# Building the guide

To build the guide, first you need to install the library.

## Installing from Hackage

Using _cabal-install_ you can install the library directly from Hackage.

    $ cabal install hatex-guide

The installed package includes a small library which exports a function
called `writeGuide`. This function has a parameter indicating the format
of the output. For example, `writeGuide LaTeX` will write the output in
the current directory in LaTeX format. Read the package documentation
to know about the supported formats.

Once the package is installed, run GHCi and run the following session.

    $ import Text.LaTeX.Guide
    $ writeGuide LaTeX

## Installing HEAD version

Run the following commands to download and install the HEAD version. _Requires git and cabal_.

    $ git clone git@github.com:Daniel-Diaz/hatex-guide.git
    $ cd hatex-guide
    $ cabal install

Once installed, import `Text.LaTeX.Guide` and use `writeGuide` to build the actual guide.
Depending on the argument used for `writeGuide`, the output will have a different format.
For example, `writeGuide LaTeX` will output in the current directory a `.tex` file of the guide.

# Contributing to the guide

There are several things to keep in mind to contribute to the guide.
If you contribute, do not forget to add your name to the `contributors` list to bound in the
`Text.LaTeX.Guide.Info` module.

## Sections

Each section of the guide is written in a different file. Every section is stored in the `src`
folder in the repository. The order in which each section appears in the guide is determined by the `sectionList`
constant defined in the `Text.LaTeX.Guide.Info` module.

## Syntax

The syntax used to write the guide is described in `Text.LaTeX.Guide.Syntax`.
The current content can also be helpful to understand it.

## Images

When including images, it is required to save them in the `res` directory, and include their file name in the
`otherResources` value defined in the `Text.LaTeX.Guide.Info` module.
