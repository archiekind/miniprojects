# von Neumann Emulator

This is an emulator of a computer that implements a von Neumann architecture based on the EDVAC. It is based on the instruction set given below. The example program given in the program.txt file sums the elements in the array beginning at line 19, memory address 18.

To compile the project, run:
```
clang -o emulator emulator.c -lm
```
and then
```
./emulator
```
to execute the program.

# Instruction Set

The instruction set was provided as unit materials as part of the Bristol BSc Computer Science course. It can be found at https://cs-uob.github.io/COMS10015/material-TB1.

| Instruction | Semantics          |
| ----------- | ------------------ |
| 00nnnn      | nop                |
| 10nnnn      | halt               |
| 20nnnn      | A <- n             |
| 21nnnn      | MEM[n] <- A        |
| 22nnnn      | A <- MEM[n]        |
| 30nnnn      | A <- A + MEM[n]    |
| 31nnnn      | A <- A - MEM[n]    |
| 32nnnn      | A <- A xor MEM[n]  |
| 40nnnn      | PC <- n            |
| 41nnnn      | PC <- n iff. A = 0 |
| 42nnnn      | PC <- n iff A != 0 |
