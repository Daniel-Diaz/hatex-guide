# HaTeX User's Guide

This is the source code of the HaTeX User's Guide. If you want to improve it, feel free to edit.
Don't forget to add your name in the `contributors` list in
[Text/LaTeX/Guide/Info.hs](https://github.com/Daniel-Diaz/hatex-guide/blob/master/Text/LaTeX/Guide/Info.hs)
module if you do so.

A pdf version (not necessarily updated) can be downloaded from [here](http://daniel-diaz.github.com/projects/hatex/hatex-guide.pdf).

# Building the guide

To build the guide, first you need to install the library.

Run the following commands to download and install the HEAD version (_requires git_).

    $ git clone git@github.com:Daniel-Diaz/hatex-guide.git
    $ cd hatex-guide
    $ cabal install

Once installed, import `Text.LaTeX.Guide` and use `writeGuide` to build the actual guide.
Depending on the argument used for `writeGuide`, the output will have a different format.
For example, `writeGuide LaTeX` will output in the current directory a `.tex` file of the guide.

# Contributing to the guide

There are several things to keep in mind to contribute to the guide.

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
