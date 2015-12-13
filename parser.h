/***********************************************************************
 * File name:       parser.h
 * Edit Compiler:   gcc version 5.1.1 20150618 IDE Code::Blocks version 13.12 on Fedora 22
 * Test Compiler:   gcc version 4.7.1 (tdm-1)  IDE Code::Blocks version 13.12 on Windows 10
 * Author:          Cory Hilliard    040 630 141 (Linux - Fedora 23)
 *                  Matthew Clements 040 766 220 (Windows - v8)
 * Course:          CST 8152 – Compilers, Lab Section: 011
 * Assignment:      04
 * Due Date:        December 11, 2015
 * Professor:       Sv. Ranev
 * Purpose:         Program to parse tokens
 * Function list:
 *                  parser(Buffer * in_buf);
 *                  match(int pr_token_code,int pr_token_attribute);
 *                  syn_eh(int sync_token_code);
 *                  syn_printe();
 *                  gen_incode(char* incode);
 *                  program();
 *                  opt_statements();
 *                  statements();
 *                  statement_prime();
 *                  statement();
 *                  assignment_statement();
 *                  assignment_expression();
 *                  selection_statement();
 *                  iteration_statement();
 *                  input_statement();
 *                  variable_list();
 *                  variable_list_prime();
 *                  output_statement();
 *                  output_list();
 *                  opt_variable_list();
 *                  arithmetic_expression();
 *                  unary_arithmetic_expression();
 *                  additive_arithmetic_expression();
 *                  additive_arithmeti_expression_prime();
 *                  multiplicative_arithmetic_expression();
 *                  multiplicative_arithmetic_expression_prime();
 *                  primary_arithmetic_expression();
 *                  string_expression();
 *                  string_expression_prime();
 *                  primary_string_expression();
 *                  conditional_expression();
 *                  logical_OR_expression();
 *                  logical_OR_expression_prime();
 *                  logical_AND_expression();
 *                  logical_AND_expression_prime();
 *                  relational_expression();
 *                  primary_a_relational_expression_list();
 *                  primary_s_relational_expression_list();
 *                  primary_a_relational_expression();
 *                  primary_s_relational_expression();
 **********************************************************************/
#ifndef PARSER_H_
#define PARSER_H_

/* header includes */
#include "buffer.h"         /* comments */
#include "stable.h"         /* comments */
#include "token.h"          /* comments */
/*#include "table.h"          /* comments */

/* defines */
#define ELSE        0       /* comments */
#define IF          1       /* comments */
#define INPUT       2       /* comments */
#define OUTPUT      3       /* comments */
#define PLATYPUS    4       /* comments */
#define REPEAT      5       /* comments */
#define THEN        6       /* comments */
#define USING       7       /* comments */
#define NO_ATTR     0       /* comments */

/* extern definitions */
extern Token mlwpar_next_token(Buffer* sc_buf);
extern STD sym_table;
extern int line;                /* line number counter */
extern Buffer * str_LTBL;       /* string literal table */
extern char * kw_table [];

/* function prototypes */
void parser(Buffer* in_buf);                            /* creates a Buffer structure and sets values with given parameters */
void match(int pr_token_code,int pr_token_attribute);   /* matches the current input token (lookahead) and the token required by the parser */
void syn_eh(int sync_token_code);                       /* implements a simple panic mode error recovery */
void syn_printe();                                      /* prints error messages */
void gen_incode(char* incode);                          /* assignment the function takes a string as an argument and prints it */

void program(void);                                     /* comments */
void opt_statements(void);                              /* comments */
void statement_prime(void);                             /* comments */
void statements(void);                                  /* comments */
void statement(void);                                   /* comments */
void assignment_statement(void);                        /* comments */
void assignment_expression(void);                       /* comments */
void selection_statement(void);                         /* comments */
void iteration_statement(void);                         /* comments */
void input_statement(void);                             /* comments */
void variable_list(void);                               /* comments */
void variable_list_prime(void);                         /* comments */
void output_statement(void);                            /* comments */
void output_list(void);                                 /* comments */
void variable_identifier(void);
void opt_variable_list(void);                           /* comments */
void arithmetic_expression(void);                       /* comments */
void unary_arithmetic_expression(void);                 /* comments */
void additive_arithmetic_expression(void);              /* comments */
void additive_arithmetic_expression_prime(void);        /* comments */
void multiplicative_arithmetic_expression(void);        /* comments */
void multiplicative_arithmetic_expression_prime(void);  /* comments */
void primary_arithmetic_expression(void);               /* comments */
void string_expression(void);                           /* comments */
void string_expression_prime(void);                     /* comments */
void primary_string_expression(void);                   /* comments */
void conditional_expression(void);                      /* comments */
void logical_OR_expression(void);                       /* comments */
void logical_OR_expression_prime(void);                 /* comments */
void logical_AND_expression(void);                      /* comments */
void logical_AND_expression_prime(void);                /* comments */
void relational_expression(void);                       /* comments */
void primary_a_relational_expression_list(void);        /* comments */
void primary_s_relational_expression_list(void);        /* comments */
void primary_a_relational_expression(void);             /* comments */
void primary_s_relational_expression(void);             /* comments */

#endif /* PARSER_H_ */
