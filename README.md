# HaTeX User's Guide

This is the source code of the HaTeX User's Guide. If you want to improve it, feel free to edit.
Don't forget to add your name in the `contributors` list in Core/Info.hs module if you do so.

A pdf version (not necessarily updated) can be downloaded from [here](http://daniel-diaz.github.com/projects/hatex/hatex-guide.pdf).

# Building the guide

To build the guide, load the UserGuide.hs module and run `writeBackend` with your desired backend (for example, `writeBackend LaTeX`).
This will create a file containing the output in the desired format.

Each section must be written in a separate file. The list of this files must be specified in the
`sectionList` value in the Core/Info.hs module.

The syntax used to write the guide is described in Core/Syntax.hs, and more detailed in the
[wiki](https://github.com/Daniel-Diaz/HaTeX-Guide/wiki/Syntax). We use the `parsec` library to parse the syntax.

**To get UserGuide.hs working you need HaTeX version 3.6.\*!**
