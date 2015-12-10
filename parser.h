/***********************************************************************
 * File name:       parser.h
 * Edit Compiler:   gcc version 5.1.1 20150618 IDE Code::Blocks version 13.12 on Fedora 22
 * Test Compiler:   gcc version 4.7.1 (tdm-1)  IDE Code::Blocks version 13.12 on Windows 10
 * Author:          Cory Hilliard    040 630 141 (Linux - Fedora 23)
 *                  Matthew Clements 040 766 220 (Windows - v8)
 * Course:          CST 8152 – Compilers, Lab Section: 011
 * Assignment:      01
 * Due Date:        December 11, 2015
 * Professor:       Sv. Ranev
 * Purpose:         Program to parse tokens
 * Function list:
 * WARNINGS:        3 Warnings in Visual Studio
 **********************************************************************/


/* function prototypes */
void parser(Buffer * in_buf);                           /* creates a Buffer structure and sets values with given parameters */
void match(int pr_token_code,int pr_token_attribute);   /* matches the current input token (lookahead) and the token required by the parser */
void syn_eh(int sync_token_code);                       /* implements a simple panic mode error recovery */
void syn_printe();                                      /* prints error messages */
void gen_incode(char* incode);                          /* assignment the function takes a string as an argument and prints it */
void program(void);                                     /* assignment the function takes a string as an argument and prints it */
void input_statement(void);                             /* assignment the function takes a string as an argument and prints it */
void opt_statements();                                  /* assignment the function takes a string as an argument and prints it */

