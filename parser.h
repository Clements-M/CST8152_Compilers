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
#include "buffer.h"
#include "stable.h"
#include "token.h"

/* keyword defines */
#define ELSE        0
#define IF          1
#define INPUT       2
#define OUTPUT      3
#define PLATYPUS    4
#define REPEAT      5
#define THEN        6
#define USING       7

/* no attribute define */
#define NO_ATTR    -1

/* extern definitions */
extern Token mlwpar_next_token(Buffer* sc_buf);         /* external scanner declaration */
extern STD sym_table;                                   /* external symbol table declaration */
extern int line;                                        /* external line number counter declaration */
extern Buffer* str_LTBL;                                /* external string table buffer pointer declaration */
extern char* kw_table [];                               /* external keyword table string declaration */

/* function prototypes */
void parser(Buffer* in_buf);                            /* creates a Buffer structure and sets values with given parameters */
void match(int pr_token_code,int pr_token_attribute);   /* matches the current input token (lookahead) and the token required by the parser */
void syn_eh(int sync_token_code);                       /* implements a simple panic mode error recovery */
void syn_printe();                                      /* prints error messages */
void gen_incode(char* incode);                          /* assignment the function takes a string as an argument and prints it */

void program(void);                                     /* parses program */
void opt_statements(void);                              /* parses optional statements */
void statement_prime(void);                             /* parses statements prime */
void statements(void);                                  /* parses statements */
void statement(void);                                   /* parses statement */
void assignment_statement(void);                        /* parses assignment statement */
void assignment_expression(void);                       /* parses assignment expression */
void selection_statement(void);                         /* parses selection statement */
void iteration_statement(void);                         /* parses iteration statement */
void input_statement(void);                             /* parses input statement */
void variable_list(void);                               /* parses variable list */
void variable_list_prime(void);                         /* parses variable list prime */
void output_statement(void);                            /* parses output statement */
void output_list(void);                                 /* parses output list */
void variable_identifier(void);                         /* parses variable identifier */
void opt_variable_list(void);                           /* parses optional variable list */
void arithmetic_expression(void);                       /* parses arithmetic expression */
void unary_arithmetic_expression(void);                 /* parses uniry arithmetic expression */
void additive_arithmetic_expression(void);              /* parses additive arithmetic expression */
void additive_arithmetic_expression_prime(void);        /* parses additive arithmetic expression prime */
void multiplicative_arithmetic_expression(void);        /* parses multiplicative arithmetic expression */
void multiplicative_arithmetic_expression_prime(void);  /* parses multiplicative arithmetic expression prime */
void primary_arithmetic_expression(void);               /* parses primary arithmetic expression */
void string_expression(void);                           /* parses string expression */
void string_expression_prime(void);                     /* parses string expression prime */
void primary_string_expression(void);                   /* parses primary string expression */
void conditional_expression(void);                      /* parses conditional expression */
void logical_OR_expression(void);                       /* parses logical OR expression */
void logical_OR_expression_prime(void);                 /* parses logical OR expression prime */
void logical_AND_expression(void);                      /* parses logical AND expression */
void logical_AND_expression_prime(void);                /* parses logical AND expression prime */
void relational_expression(void);                       /* parses relational expression */
void primary_a_relational_expression_list(void);        /* parses primary a relational expression list */
void primary_s_relational_expression_list(void);        /* parses primary s relational expression list */
void primary_a_relational_expression(void);             /* parses primary a relational expression */
void primary_s_relational_expression(void);             /* parses primary s relational expression */

#endif /* PARSER_H_ */
