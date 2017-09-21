/***********************************************************************
 * File name:       stable.h
 * Compiler:        gcc
 * IDE:             Code::Blocks version 13.12 on (mixed environments)
 * Authors:         Cory Hilliard    040 630 141 (Linux - Fedora 23)
 *                  Matthew Clements 040 766 220 (Windows - v8)
 * Course:          CST8152 - Compilers, Lab Section: 011
 * Assignment:      03 Symbol Table
 * Due Date:        Friday November 20 2015
 * Professor:       Svillen Ranev
 * Purpose:         Implementing and Incorporating a Symbol Table
 **********************************************************************/
#ifndef STABLE_H_
#define STABLE_H_

#include "buffer.h"

/* union used for holding initial value of the record */
typedef union InitialValue {
	int int_val;                 /* integer variable initial value */
	float fpl_val;               /* floating-point variable initial value */
	int str_offset;              /* string variable initial value (offset) */
} InitialValue;

/* structure used to hold individual records of the lexeme */
typedef struct SymbolTableVidRecord {
	unsigned short status_field; /* variable record status field*/
	char * plex;                 /* pointer to lexeme (VID name) in CA */
	int o_line;                  /* line of first occurrence */
	InitialValue i_value;        /* variable initial value */
	size_t reserved;             /* reserved for future use*/
} STVR;

/* structure to hold information about every record */
typedef struct SymbolTableDescriptor {
	STVR *pstvr;                 /* pointer to array of STVR */
	int st_size;                 /* size in number of STVR elements */
	int st_offset;               /* offset in number of STVR elements */
	Buffer *plsBD;               /* pointer to the lexeme storage buffer descriptor */
} STD;

/* MASKS*/
/* 16 bit field  MSB-> 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0 <-LSB */
#define RESET_ALL   0x0000 /* 0000 0000 0000 0000 */
/*TODO: THESE MUST BE REVERSED :( */
#define RESET_0201  0x0001 /* 0000 0000 0000 0001 */
#define RESET_00    0x0006
/*NOOOOOOOOOOOOOOOOOOOOOOOOOOO*/
#define DEFAULT     0xFFF8
#define SET_00_0    0xFFF8
#define SET_00_1    0xFFF9
#define SET_21_00   0xFFF8
#define SET_21_01   0xFFFA
#define SET_21_10   0xFFFC
#define SET_21_11   0xFFFE
#define CHK_LSB     0x0001
#define CHK_21      0xFFF9
#define CHK_FLT     0xFFFB
#define CHK_INT     0xFFFD
#define CHK_STR     0xFFFF

#define INIT_CAPACITY 200   /* initial buffer capacity */
#define INC_FACTOR    15    /* increment factor */

#define FAIL R_FAIL_1

/* forward declarations */
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table,int vid_offset,char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type (STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
int st_sort(STD sym_table, char s_order);
int st_store(STD sym_table);

#endif /* STABLE_H */
