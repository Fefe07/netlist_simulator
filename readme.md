#Netlist simulator

This project was realised for a digital systems course.  

## How to use
- To build the project :  
```bash
ocamlbuild netlist_simulator.byte
```
- To run on a test :  
```bash
./netlist_simulator.byte -n number_of_steps path_of_the_file
```
You can find test files in the test directory.  

## Implementation choices

RAM and ROM : there are as many RAM and ROMs as RAM and ROM queries in the code.  
The ROM is filled by the user in the shell at the launching of the code, but it could be read in external files too.  

## Misc
The prebuilt\_netlist\_simulator.byte file served as a model for basic features, it is left here if needed for debugging.  
