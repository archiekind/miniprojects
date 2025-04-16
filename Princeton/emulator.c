#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

void initialiseMemory(uint32_t* memory) {
  FILE* file = fopen("program.txt", "r");
  if (file == NULL) {
    fprintf(stderr, "Error opening file\n");
    exit(1);
  }

  for (int address = 0; !feof(file); address++) {
    uint32_t data = 0;
    int newline = 0;
    while (1) {
      char c = fgetc(file);
      if (c == '\n' || feof(file)) {
        newline = 1;
        break;
      }
      if (c - '0' > 9 || c - '0' < 0) break;
      data *= 10;
      data += c - '0';
    }
    if (!newline) while (fgetc(file) != '\n' && !feof(file));
    memory[address] = data;
  }
  fclose(file);
}

int main() {
  // initialise memory
  uint32_t* memory = (uint32_t*) malloc(10000 * sizeof(uint32_t));
  initialiseMemory(memory);
  
  uint32_t pc = 0; // program counter
  uint32_t a = 0; // accumulator
  uint32_t ir = 0; // instruction register

  // debug
  int debug = 1;

  while (1) {
    // fetch
    ir = memory[pc];
    pc++;

    printf("PC: %d, IR: %d\n", pc - 1, ir);
    // decode + execute
    switch ((int) floor(ir / 10000)) {
      case 0: // no operation
        break;
      case 10: // halt
        if (debug) printf("Halting execution.\n");
        printf("Final value in accumulator: %d\n", a);
        free(memory);
        return 0;
      case 20: // load immediate
        a = ir % 10000;
        if (debug) printf("Loaded immediate value: %d\n", a);
        break;
      case 21: // store
        memory[ir % 10000] = a;
        if (debug) printf("Stored value %d at address %d\n", a, ir % 10000);
        break;
      case 22: // load from memory
        a = memory[ir % 10000];
        if (debug) printf("Loaded value %d from address %d\n", a, ir % 10000);
        break;
      case 30: // add
        a = a + memory[ir % 10000];
        if (debug) printf("Added value from address %d, new accumulator value: %d\n", ir % 10000, a);
        break;
      case 31: // subtract
        a = a - memory[ir % 10000];
        if (debug) printf("Subtracted value from address %d, new accumulator value: %d\n", ir % 10000, a);
        break;
      case 32: // XOR
        a = a ^ memory[ir % 10000];
        if (debug) printf("XORed value from address %d, new accumulator value: %d\n", ir % 10000, a);
        break;
      case 40: // jump
        pc = memory[ir % 10000];
        if (debug) printf("Jumping to address %d\n", pc);
        break;
      case 41: // jump if zero
        if (a == 0) {
          pc = ir % 10000;
          if (debug) printf("Jumping to address %d because accumulator is zero\n", pc);
        } else {
          if (debug) printf("Not jumping, accumulator is not zero\n");
        }
        break;
      case 42: // jump if not zero
        if (a != 0) {
          pc = ir % 10000;
          if (debug) printf("Jumping to address %d because accumulator is not zero\n", pc);
        } else {
          if (debug) printf("Not jumping, accumulator is zero\n");
        }
        break;
    }
  }

  return 0;
}