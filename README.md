# CST8152 PLATYPUS Compiler

Completed front end of PLATYPUS compiler for Professor Svillen Ranev's CST8152 course at Algonquin College.

This project consisted of 4 major parts.

1) Buffer.c

Dynamic data structure that will manage it's size depending on growth method. A text file is read and each character is added to the data structure. When the buffer reaches capacity it will determine to grow by a set increment (additive mode), a percentage of it's current size (multiplicative mode), or not grow at all (fixed mode). The buffer structure (buffer.h) consists of a pointer to the char array, various offsets used when in use, size, and other information about the buffer when in use.

2) Scanner.c

Scanner used to read and process data from the buffer. This is a finite state machine that groups the characters found in the buffer into tokens, identifying possible operators, variable identifiers, keywords, etc. Each token contains token attributes such as variable name, numerical values, or offsets pointing to strings in a buffer. Tokens and token attribute structures can be found in Token.h.

3) Stable.c

Symbol table for managing the information found by the scanner. The symbol table is a simple database of variable identifiers found by scanner.c, it contains information such as variable name, value, type, and line number (used if errors occur in input PLATYPUS code). The symbol table descriptor structure (stable.h) contains information about the symbol table itself, size, pointer to the start, and offsets, while the symbol table record structure contains information about the individual records.

4) Parser.c

Top-down LL parser for ensuring grammatical correctness of input code. This task started by modifying a supplied grammar from LR to LL, left factoring and removing left recursion. The modified grammar can be found in the submission folder. With the grammar completed, parser.c was created. It moves from token to token identifying various structures such as for loops, if/else clauses, or relational expressions. The parser starts when it encounters the PLATYPUS starting token, and loops until it reaches and end-of-file (EOF) token.

Source files are:

buffer.c

buffer.h

scanner.c

table.h

token.h

stable.c

stable.h

parser.c

parser.h

platy.c

Main function found in platy.c.


Once compiled the program should be run with "./(executable) (source code path) > (output path)" where the source code path points to an "Assignment4xyz.pls" PLATYPUS language source file.
