# practyp

Simple tool to type in your favourite terminal.

## Installation

As I am not a professional developer, currently the way to run the code is using SBT.
Once the installation is done (refer to [SBT website](https://www.scala-sbt.org/download.html)), 
go to practyp folder and run 
```
sbt run
```
The program will dislpay some words you have to type. At the end of the input (return) your stats will be shown.
Currently only Spanish words are used.

## Extending words

Currently, due to the early stage of the program, to extend it for using other words than Spanish ones, you
should modifiy the file `src/main/scala/typer/words.scala`.
