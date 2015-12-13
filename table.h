/***********************************************************************
 * File name:       table.h
 * Compiler:        gcc
 * IDE:             Code::Blocks version 13.12 on (mixed environments)
 * Authors:         Cory Hilliard    040 630 141 (Linux - Fedora 22)
 *                  Matthew Clements 040 766 220 (Windows - v8)
 * Course:          CST8152 - Compilers, Lab Section: 011
 * Assignment:      02 Lexical Analyzer (Scanner)
 * Due Date:        Tuesday October 27 2015
 * Professor:       Svillen Ranev
 * Purpose:         Functions declarations and tables for the Lexical Analyzer (Scanner)
 * Function list:   aa_func02() (declaration)
 *                  aa_func03() (declaration)
 *                  aa_func05() (declaration)
 *                  aa_func08() (declaration)
 *                  aa_func10() (declaration)
 *                  aa_func12() (declaration)
 *                  aa_func13() (declaration)
 * Table list:      st_table[ ] transition table/state table
 *                  as_table[ ] accepting state table
 *                  aa_table[ ] accepting action table (used with Token functions)
 *                  kw_table[ ] keyword lookup table
 **********************************************************************/
#ifndef  TABLE_H_
#define  TABLE_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h>      /* NULL pointer constant is defined there */
#endif

#define SEOF1    '\0'   /* Source end-of-file (SEOF) sentinel symbol '\0' */
#define SEOF2    0xFF   /* Source end-of-file (SEOF) sentinel symbol 0xFF */

#define ES      12      /* Error state */
#define IS      -1      /* Inavalid state */

/* State transition table definition */
#define TABLE_COLUMNS 7 /* column numbers in the state table */

/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
    /* State  0 */  {  1,  6,  4,  4, IS, IS, IS },
    /* State  1 */  {  1,  1,  1,  1,  2,  3,  2 },
    /* State  2 */  { IS, IS, IS, IS, IS, IS, IS },
    /* State  3 */  { IS, IS, IS, IS, IS, IS, IS },
    /* State  4 */  { ES,  4,  4,  4,  7,  5,  5 },
    /* State  5 */  { IS, IS, IS, IS, IS, IS, IS },
    /* State  6 */  { ES, ES,  9, ES,  7, ES,  5 },
    /* State  7 */  {  8,  7,  7,  7,  8,  8,  8 },
    /* State  8 */  { IS, IS, IS, IS, IS, IS, IS },
    /* State  9 */  { ES,  9,  9, ES, ES, ES, 10 },
    /* State 10 */  { IS, IS, IS, IS, IS, IS, IS },
    /* State 11 */  { IS, IS, IS, IS, IS, IS, IS },
    /* State 12 */  { IS, IS, IS, IS, IS, IS, IS },
    /* State 13 */  { IS, IS, IS, IS, IS, IS, IS }
};

/* Accepting state table definition */
#define ASWR    2       /* accepting state with retract */
#define ASNR    3       /* accepting state with no retract */
#define NOAS    0       /* not accepting state */
int as_table[ ] = {
    /* State  0 */  NOAS,
    /* State  1 */  NOAS,
    /* State  2 */  ASWR,
    /* State  3 */  ASNR,
    /* State  4 */  NOAS,
    /* State  5 */  ASWR,
    /* State  6 */  NOAS,
    /* State  7 */  NOAS,
    /* State  8 */  ASWR,
    /* State  9 */  NOAS,
    /* State 10 */  ASWR,
    /* State 11 */  NOAS,
    /* State 12 */  ASNR,
    /* State 13 */  ASWR
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func12(char *lexeme);
Token aa_func13(char *lexeme);

/* defining a new type: pointer to function (of one char * argument) returning Token */
typedef Token (*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
PTR_AAF aa_table[ ] ={
    /* State  0 */  NULL,
    /* State  1 */  NULL,
    /* State  2 */  aa_func02,
    /* State  3 */  aa_func03,
    /* State  4 */  NULL,
    /* State  5 */  aa_func05,
    /* State  6 */  NULL,
    /* State  7 */  NULL,
    /* State  8 */  aa_func08,
    /* State  9 */  NULL,
    /* State 10 */  aa_func10,
    /* State 11 */  NULL,
    /* State 12 */  aa_func12,
    /* State 13 */  aa_func13
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  8
char * kw_table []= {
    "ELSE",
    "IF",
    "INPUT",
    "OUTPUT",
    "PLATYPUS",
    "REPEAT",
    "THEN",
    "USING"
};

#endif
