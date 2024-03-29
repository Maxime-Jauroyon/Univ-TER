# TER

The goal of the TER is to parse a data file of reactions (and inhibitions)
and construct a graph (Nodes: reactions, Edges: molecules relations)
in order to give only the reactions who can be used for a path between 2 molecules.


## Features

- Parses one type of file:
	- A list of reactions and inhibitions.
- Print the input document (without commentary and empty lines)
- Print the list of reactions and inhibitors linked to their reactions
- Specify the length of the path to find, up to 6 (will return the corresponding list of reactions)
- Display the program's version
- Specify the output file (default: stdout)
- Can choose to discard inhibitors in the searching process

## Compile And Run

The project uses dune as a cross-platform build system.

- Open a terminal in the project's root directory.
- Run `./automata`, the project should compile and run automatically.

## Architecture

Tree of the most important files and folder in the project's repository hierarchy (not everything is shown):

```
/
├─┬─src/: Sources code.  
│ └─┬─ainterpreter.ml/: Graph path finding algorithms.
│   ├─alexer.mll/: Lexing.
│   ├─aparser.mly/: Parsing.
│   ├─aprint.ml/: Printing functions.
│   ├─astring.ml/: ToString functions.
│   ├─atransitions.ml/: Transition to a graph functions.
│   ├─atypes.ml/: Types and global variables.
│   ├─autil.ml/: Utility functions.
│   ├─automata.ml/: The program.
│   └─*.*: Other files.
├─┬─tests/:
│ └───*.*: test files.
├─automata: The executable.
├─README.md: This file.
├─brenda.ssa: Data file.
└─*.*: Other files.
```

## Contributors

### [JAUROYON Maxime](https://github.com/Maxime-Jauroyon)

- used [gasp2 project](https://github.com/Maxime-Jauroyon/Univ-Gasp2) as a baseline
- refactor syntactic and lexical parser to match the data given (brenda.ssa)
- refactor the rest of the project to search a path in a graph 

## License

This project is made for educational purposes only and any part of it can be used freely.
