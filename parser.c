/***********************************************************************
 * File name:       parser.c
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

#include <stdio.h>
#include <stdlib.h>

#include "parser.h"

/*
    Define two static global variables: lookahead of type
    Token, and sc_buf of type pointer to Buffer. Additionally define a global variable
    synerrno of type int. You may add additional variable declarations and constants
    definitions, if necessary (and it is).

    Then declare the functions used in the parser
    implementation. All function prototypes, variable definitions/declarations, and constant
    definitions must be in parser.h.
*/

/* global variables */
static Token lookahead;        /* look ahead token to match with ... */
static Buffer* sc_buf;         /* a pointer to a buffer for the source file */
int    synerrno;               /* for error counting */


/***********************************************************************
 * Purpose:             creates a Buffer structure and sets values with given parameters
 * Author:              King Svillen Ranev of the round table
 * History/Versions:    1.0
 * Called functions:    mlwpar_next_token()
 *                      program()
 *                      match()
 *                      gen_incode()
 * Parameters:          in_buf  buffer pointer  points to a buffer structure
 * Return value:        none
 * Algorithm:           test for errors if true return null
 *                      create Buffer (pBD) and char array (pCharArray) in dynamic memory
 *                      sets the member values according to given parameters
 *                      return pointer to a Buffer (pBD)
 **********************************************************************/
void parser(Buffer* in_buf) {

    sc_buf = in_buf;
    lookahead = mlwpar_next_token(sc_buf);
    program();
    match(SEOF_T,NO_ATTR);
    gen_incode("PLATY: Source file parsed");

}


/***********************************************************************
 * Purpose:             matches the current input token (lookahead) and the token required by the parser
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    malloc()
 *                      free()
 *                      calloc()
 * Parameters:          pr_token_code:      int   (ZERO to SHRT_MAX)
 *                      pr_token_attribute: int   (multiplicative: MIN_RANGE_1 to MAX_RANGE_100)
 *                                              (addative: 1 to 255)
 *                      o_mode:         char    (f, a, m)
 * Return value:        none
 * Algorithm:           Step 3:
 *                      Write your match() function. The prototype for the function is:
 *                      The match() function matches two tokens: the current input token (lookahead) and the
 *                      token required by the parser. The token required by the parser is represented by two
 *                      integers - the token code (pr_token_code), and the token attribute
 *                      (pr_token_attribute). The attribute code is used only when the token code is one of
 *                      the following codes: KW_T, LOG_OP_T, ART_OP_T, REL_OP_T. In all other cases
 *                      the token code is matched only.
 *                      If the match is successful and the lookahead is SEOF_T, the function returns.
 *                      If the match is successful and the lookahead is not SEOF_T, the function advances to
 *                      the next input token by executing the statement:
 *                      lookahead = mlwpar_next_token (sc_buf);
 *                      If the new lookahead token is ERR_T, the function calls the error printing function
 *                      syn_printe(), advances to the next input token by calling mlwpar_next_token () again,
 *                      increments the error counter synerrno, and returns.
 *                      If the match is unsuccessful, the function calls the error handler
 *                      syn_eh(pr_token_code) and returns.
 *
 *                      Note: Make your match() function as efficient as possible. This function is called many
 *                      times during the parsing. The function will be graded with respect to design and
 *                      efficiency.
 **********************************************************************/
void match(int pr_token_code,int pr_token_attribute) {

	/*If the token code itself is not a match, no need to check attribute */
	if ( pr_token_code != lookahead.code )
	{
		syn_eh(pr_token_code);
		return;
	}
	
	/*Enter with matching code, check the 4 if the attribute is valid */
	switch ( pr_token_code ) {
	case KW_T:
		if( pr_token_attribute != lookahead.attribute.int_value)
		{
			syn_eh(pr_token_code);
			return;
		}	
		break;
	case LOG_OP_T:
		if( pr_token_attribute != lookahead.attribute.log_op)
		{
			syn_eh(pr_token_code);
			return;
		}	
		break;
	case ART_OP_T:
		if( pr_token_attribute != lookahead.attribute.arr_op)
		{
			syn_eh(pr_token_code);
			return;
		}	
		break;
	case REL_OP_T:
		if( pr_token_attribute != lookahead.attribute.rel_op)
		{
			syn_eh(pr_token_code);
			return;
		}	
		break;
	}
	/* Matching code, and matching attribute at this point */

	/*Successful match, now check if lookahead is SEOF */
	if ( lookahead.code == SEOF_T )
		return;

	lookahead = mlwpar_next_token (sc_buf);	
	if( lookahead.code == ERR_T )
	{
			syn_printe();
			lookahead = mlwpar_next_token (sc_buf);
			++synerrno;
			return;
	}	
}


/***********************************************************************
 * Purpose:             implements a simple panic mode error recovery.
 * Author:
 * History/Versions:    1.0
 * Called functions:    malloc()
 *                      free()
 *                      calloc()
 * Parameters:          init_capacity:  short   (ZERO to SHRT_MAX)
 *                      inc_factor:     char    (multiplicative: MIN_RANGE_1 to MAX_RANGE_100)
 *                                              (addative: 1 to 255)
 *                      o_mode:         char    (f, a, m)
 * Return value:        pointer to Buffer
 * Algorithm:           Step 4:
 * First set:           FIRST, the function calls syn_printe() and increments the error counter. Then the
 *                      function implements a panic mode error recovery: the function advances the input token
 *                      (lookahead) until it finds a token code matching the one required by the parser
 *                      (pr_token_code passed to the function as sync_token_code ).
 *                      It is possible, when advancing, that the function can reach the end of the source file
 *                      without finding the matching token. To prevent from overrunning the input buffer, before
 *                      every move the function checks if the end of the file is reached. If the function looks for
 *                      sync_token_code different from SEOF_T and reaches the end of the source file, the
 *                      function calls exit(synerrno).
 *                      If a matching token is found and the matching token is not SEOF_T, the function
 *                      advances the input token one more time and returns. If a matching token is found and
 *                      the matching token is SEOF_T, the function returns.
 **********************************************************************/
void syn_eh(int sync_token_code){

	/*calls syn_printe() and increments the error counter*/
	syn_printe();
	++synerrno;

	/*sync up the parser by finding the next occurence of sync token*/
	while (lookahead.code != sync_token_code && lookahead.code != SEOF_T)
		lookahead = mlwpar_next_token(sc_buf);

	/*function looks for sync_token_code different from SEOF_T
	and reaches the end of the source file, the function calls exit(synerrno)*/
	if ( sync_token_code != SEOF_T )
		exit(synerrno);

	/*matching token is not SEOF_T, the function advances the input token one more time */
	if ( lookahead.code != SEOF_T )
		lookahead = mlwpar_next_token(sc_buf);

	/*while loop ensured that matching token is found and if match is SEOF_T breaks loop/gets to this point */

}


/***********************************************************************
 * Purpose:             Parser error printing function, Assignmet 4, F15
 * Author:              King Svillen Ranev of the round table
 * History/Versions:    1.0
 * Called functions:    printf()
 *                      b_setmark()
 * Parameters:          none
 * Algorithm:           Step 5:
 *                      Write the error printing function syn_printe() .
 *                      void syn_printe()
 *                      Note: This function implementation is provided for you in Assignment4MPTF.zip.
 *
 *                      The function prints the following error message:
 *                      PLATY: Syntax error: Line: line_number_of_the_syntax_error
 *                      ***** Token code:lookahead token code Attribute: token attribute
 *                      and returns. For example:
 *                      PLATY: Syntax error: Line: 2
 *                      *****  Token code: 13 Attribute: NA
 *                      PLATY: Syntax error: Line: 8
 *                      *****  Token code: 9 Attribute: 0
 *
 *                      If the offending token is a keyword, variable identifier or string literal you must use the
 *                      corresponding token attribute to access and print the lexeme (keyword name, variable
 *                      name, or string).
 *                      For example, to print the keyword lexeme you must use the kw_table defined in
 *                      table.h. Important note: You are not allowed to copy the keyword table in parser.h or
 *                      parser.c. You must use a proper declaration to create a link to the one defined in
 *                      table.h.
 *                      Similarly, you must use the symbol table or the string literal table to print the variable
 *                      names or the sting literals.
 **********************************************************************/
/* Parser error printing function, Assignmet 4, F15
 */
void syn_printe(){
Token t = lookahead;

printf("PLATY: Syntax error:  Line:%3d\n",line);
printf("*****  Token code:%3d Attribute: ", t.code);
switch(t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n",t.attribute.err_lex);
	 break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n" );
	 break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T :/* SVID_T    3  String Variable identifier token */
		printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
	 break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n",t.attribute.flt_value);
	 break;
	case INL_T: /* INL_T      5   Integer literal token */
	        printf("%d\n",t.attribute.get_int);
	 break;
	case STR_T:/* STR_T     6   String literal token */
	        printf("%s\n",b_setmark(str_LTBL,t.attribute.str_offset));
	break;
        
        case SCC_OP_T: /* 7   String concatenation operator token */
	        printf("NA\n" );
	break;
	
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n" );
	break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n",t.attribute.get_int);
	break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */ 
		printf("%d\n",t.attribute.get_int);
	break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n",t.attribute.get_int);
	break;
	
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n" );
	break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
	        printf("NA\n" );
	break;
	case LBR_T: /*    14   Left brace token */
	        printf("NA\n" );
	break;
	case RBR_T: /*    15  Right brace token */
	        printf("NA\n" );
	break;
		
	case KW_T: /*     16   Keyword token */
	        printf("%s\n",kw_table [t.attribute.get_int]);
	break;
	
	case COM_T: /* 17   Comma token */
	        printf("NA\n");
	break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
	        printf("NA\n" );
	break; 		
	default:
	        printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }/*end switch*/
}/* end syn_printe()*/


/***********************************************************************
 * Purpose:             assignment the function takes a string as an argument and prints it
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    printf()
 * Parameters:          incode:     char*
 * Return value:        none
 * Algorithm:           Step 6:
 *                      Write the gen_incode() function. In Part 1 of this assignment the function takes a string
 *                      as an argument and prints it. Later the function can be modified and used to emit
 *                      intermediate (Bonus 1) or machine code. The function may be called any time a
 *                      production is recognized (see parser()). The format of the message is: “PLATY:
 *                      Program parsed”, “PLATY: Assignment statement parsed”, and so on (see the sample
 *                      output files).
 **********************************************************************/
void gen_incode(char* incode){

    /* print incode string */
    printf("%s\n", incode);

}


/***********************************************************************
 * Purpose:             assignment the function takes a string as an argument and prints it
 * Author:              King Svillen Ranev of the round table
 * History/Versions:    1.0
 * Called functions:    malloc()
 *                      free()
 *                      calloc()
 * Parameters:          init_capacity:  short   (ZERO to SHRT_MAX)
 *                      inc_factor:     char    (multiplicative: MIN_RANGE_1 to MAX_RANGE_100)
 *                                              (addative: 1 to 255)
 *                      o_mode:         char    (f, a, m)
 * Return value:        pointer to Buffer
 * Algorithm:           Step 7:
 *                      For each of your grammar productions write a function named after the name of the
 *                      production. For example:
 *                      void program(void){
 *                          match(KW_T,PLATYPUS);match(LBR_T,NO_ATTR);opt_statements();
 *                          match(RBR_T,NO_ATTR);
 *                          gen_incode("PLATY: Program parsed");
 *                      }
 *                      Writing a production function, follow the substeps below.
 **********************************************************************/

 /***********************************************************************
 * Purpose:             for parsing program
 * Author:              King Svillen Ranev of the round table
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <program> ->
 *                          PLATYPUS { <opt_statements> } SEOF
 * First set:           FIRST(<program>) = {PLATYPUS}
 **********************************************************************/
void program(void) {

    match(KW_T,PLATYPUS);
    match(LBR_T,NO_ATTR);
    opt_statements();
    match(RBR_T,NO_ATTR);
    gen_incode("PLATY: Program parsed");

}


/***********************************************************************
 * Purpose:             assignment the function takes a string as an argument and prints it
 * Algorithm:           Step 7.2:
 *                      If a production has more than one alternatives on the right side (even if one of them is
 *                      empty), you must use the FIRST set for the production.
 *                      For example, the FIRST set for the <opt_statements> production is: { KW_T (but not
 *                      PLATYPUS, ELSE, THEN, REPEAT), AVID_T, SVID_T, and ϵ.
 *                      Here is an example how the FIRST set is used to write a function for a production:
 *                      void opt_statements(){
 *                          FIRST set: {AVID_T,SVID_T,KW_T(but not ... see above),ϵ}
 *
 *                          switch(lookahead.code){
 *                              case AVID_T:
 *                              case SVID_T: statements();break;
 *                              case KW_T:
 *                                  check for PLATYPUS, ELSE, THEN, REPEAT here and in statements_p()
 *                          if (lookahead. attribute. get_int != PLATYPUS
 *                              && lookahead. attribute. get_int != ELSE
 *                              && lookahead. attribute. get_int != THEN
 *                              && lookahead. attribute. get_int != REPEAT){
 *                                  statements();
 *                                  break;
 *                          }
 *
 *                              default: empty string – optional statements ;
 *                              gen_incode("PLATY: Opt_statements parsed");
 *                          }
 *                      }
 *
 *                      Pay special attention to the implementation of the empty string. If you do not have an
 *                      empty string in your production, you must call the syn_printe() function at that point .
 *                      IMPORTANT NOTE: You are not allowed to call the error handling function syn_eh()
 *                      inside the production functions and you are not allowed to advance the lookahead
 *                      within the production functions as well. Only match() can call syn_eh(), and only
 *                      match() and syn_eh() can advance lookahead.
 *                      ANOTHER NOTE: Each function must contain a line in its header indicating the
 *                      production it implements and the FIRST set for that production (see above).
 **********************************************************************/
/***********************************************************************
 * Purpose:             for parsing opt_statements
 * Author:              King Svillen Ranev of the round table
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <opt_statements> ->
 *                          <statements> | ϵ
 * First set:           FIRST(<opt_statements>) = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, ϵ}
 **********************************************************************/
void opt_statements() {

    switch(lookahead.code) {

        case AVID_T:
        case SVID_T:
            statements();
            break;

        case KW_T:
            /* check for PLATYPUS, ELSE, THEN, REPEAT here and in statements_p() */
            switch (lookahead.attribute.kwt_idx) {
                case PLATYPUS:
                case ELSE:
                case THEN:
                case REPEAT:
                    statements();
                    break;
            }

    }

    gen_incode("PLATY: Opt_statements parsed");

}


/***********************************************************************
 * Purpose:             for parsing statements
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <statements> ->
 *                          <statement> <statement_prime>
 * First set:           FIRST(<statements>) = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT}
 **********************************************************************/
void statements(void) {

    statement();
    statement_prime();

    gen_incode("PLATY: Statements parsed");

}


/***********************************************************************
 * Purpose:             for parsing statement prime
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <statement_prime> ->
 *                          <statement> <statement_prime> | ϵ
 * First set:           FIRST(<statement_prime>) = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, ϵ}
 **********************************************************************/
void statement_prime(void) {

    switch(lookahead.code) {

        case KW_T:
            switch(lookahead.attribute.kwt_idx) {

                case IF:
                case USING:
                case INPUT:
                case OUTPUT:
                    break;

                /* any other keyword */
                default:
                    return;

            }

        case AVID_T:
        case SVID_T:
            statement();
            statement_prime();
            break;
    }

    /* print even if epsilon */
    gen_incode("PLATY: Statement prime parsed");

}


/***********************************************************************
 * Purpose:             for parsing statement
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <statement> ->
 *                          <assignment_statement> | <selection_statement> | <iteration_statement> |
 *                          <input_statement> | <output_statement>
 * First set:           FIRST(<statement>) = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT}
 **********************************************************************/
void statement(void) {

    switch(lookahead.code) {

        case AVID_T:
        case SVID_T:
            assignment_statement();
            break;

        case KW_T:
            switch(lookahead.attribute.kwt_idx) {

                case IF:
                    selection_statement();
                    break;

                case USING:
                    iteration_statement();
                    break;

                case INPUT:
                    input_statement();
                    break;

                case OUTPUT:
                    output_statement();
                    break;

                /* print error on other keywords */
                default:
                    syn_printe();
                    return;
            }
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;
    }

    gen_incode("PLATY: Statement parsed");

}


/***********************************************************************
 * Purpose:             for parsing assignment statement
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <assignment_statement> ->
 *                          <assignment_expression>;
 * First set:           FIRST(<assignment_statement>) = {AVID_T, SVID_T}
 **********************************************************************/
void assignment_statement(void) {

    assignment_expression();
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Assignment statement parsed");

}


/***********************************************************************
 * Purpose:             for parsing assignment expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <assignment_expression> ->
 *                          AVID_T = <arithmetic_expression> | SVID_T = <string_expression>
 * First set:           FIRST(<assignment_expression>) = {AVID_T, SVID_T}
 **********************************************************************/
void assignment_expression(void) {

    switch (lookahead.code) {

        case SVID_T:
            match(SVID_T, NO_ATTR);
            match(ASS_OP_T, NO_ATTR);
            string_expression();
            break;

        case AVID_T:
            match(AVID_T, NO_ATTR);
            match(ASS_OP_T, NO_ATTR);
            arithmetic_expression();
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Assignment expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing selection statement
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <selection_statement> ->
 *                      	IF ( <conditional_expression> ) THEN <opt_statements>
 *                      	ELSE { <opt_statements> };
 * First set:           FIRST(<selection_statement>) = {IF}
 **********************************************************************/
void selection_statement(void) {

    match(KW_T, IF);
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, THEN);
    opt_statements();
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Selection statement parsed");

}


/***********************************************************************
 * Purpose:             for parsing iteration statement
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <iteration_statement> ->
 *                      	USING ( <assignment_expression>, <conditional_expression>,
 *                      	<assignment_expression> ) REPEAT { <opt_statements> };
 * First set:           FIRST(<iteration_statement>) = {USING}
 **********************************************************************/
void iteration_statement(void) {

    match(KW_T, USING);
    match(LPR_T, NO_ATTR);
    assignment_expression();
    match(COM_T, NO_ATTR);
    conditional_expression();
    match(COM_T, NO_ATTR);
    assignment_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Iteration statement parsed");

}

/***********************************************************************
 * Algorithm:           Step 7.1:
 *                      To implement the Parser, you must use the modified grammar (see Task 1). Before
 *                      writing a function, analyze carefully the production. If the production consists of a single
 *                      production rule (no alternatives), write the corresponding function without using the
 * First set:           FIRST set (see above). You can use the lookahead to verify in advance whether to
 *                      proceed with the production or to call syn_printe() function. If you do so, your output
 *                      might report quite different syntax errors than my parser will reports.
 *                      CST8152 – Compilers – Assignment 4, F15
 *                      Example: The production:
 *                      <input statement> ->
 *                          INPUT (<variable list>);
 *
 *                      MUST be implemented as follows:
 *
 *                      void input_statement(void){
 *                          match(KW_T,INPUT);
 *                          match(LPR_T,NO_ATTR);
 *                          variable_list();
 *                          match(RPR_T,NO_ATTR);
 *                          match(EOS_T,NO_ATTR);
 *                          gen_incode("PLATY: Input statement parsed");
 *                      }
 *
 *                      AND MUST NOT be implemented as shown below:
 *
 *                      void input_statement(void){
 *                          if(lookahead.code == KW_T
 *                          && lookahead. attribute. get_int== INPUT) {
 *                          match(KW_T,INPUT);
 *                          match(LPR_T,NO_ATTR);
 *                          variable_list();
 *                          match(RPR_T,NO_ATTR);
 *                          match(EOS_T,NO_ATTR);
 *                          gen_incode("PLATY: Input statement parsed");
 *                      }else
 *                          syn_printe();
 *                      }
 *                      This implementation will “catch” the syntax error but will prevent the match() function
 *                      from calling the error handler at the right place.
 **********************************************************************/
/***********************************************************************
 * Purpose:             for parsing input statement
 * Author:              King Svillen Ranev of the round table
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <input_statement> ->
 *  	                    INPUT ( <variable_list> );
 * First set:           FIRST(<input_statement>) = {INPUT}
 **********************************************************************/
void input_statement(void) {

    match(KW_T,INPUT);
    match(LPR_T,NO_ATTR);
    variable_list();
    match(RPR_T,NO_ATTR);
    match(EOS_T,NO_ATTR);

    gen_incode("PLATY: Input statement parsed");

}


/***********************************************************************
 * Purpose:             for parsing variable list
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    variable_identifier(), variable_list_prime()
 * Parameters:          none
 * Return value:        none
 * Production:          <variable_list> ->
 *                      	<variable_identifier> <variable_list_prime>
 * First set:           FIRST(<variable_list>) = {AVID_T, SVID_T}
 **********************************************************************/
void variable_list(void) {

    variable_identifier();
    variable_list_prime();

    gen_incode("PLATY: Variable list parsed");

}

/***********************************************************************
 * Purpose:             for parsing variable identifier
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    match()
 * Parameters:          none
 * Return value:        none
 * Production:          <variable _identifier> ->
 *                      	AVID_T | SVID_T
 * First set:           FIRST(<variable_identifier>) = {AVID_T, SVID_T}
 **********************************************************************/
void variable_identifier(void) {
    switch(lookahead.code) {

        case AVID_T:
        case SVID_T:
            match(lookahead.code, NO_ATTR);
            break;

        /* print error on anything else */
        default:
            syn_printe();

    }
}




/***********************************************************************
 * Purpose:             for parsing variable list prime
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <variable_list_prime> ->
 *                      	,<variable_list> <variable_list_prime> | ϵ
 * First set:           FIRST(<variable_list_prime>) = {, ϵ}
 **********************************************************************/
void variable_list_prime(void) {

    /* test for comma, else epsilon */
    if (lookahead.code == COM_T) {

        match(lookahead.code, lookahead.attribute.arr_op);
        variable_identifier();
        variable_list_prime();

    }

    gen_incode("PLATY: Variable list prime parsed");

}


/***********************************************************************
 * Purpose:             for parsing output statement
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <output_statement> ->
 *                      	OUTPUT (<output_list>);
 * First set:           FIRST(<output_statement>) = {OUTPUT}
 **********************************************************************/
void output_statement(void) {

    match(KW_T, OUTPUT);
    match(LPR_T, NO_ATTR);
    output_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Output statement parsed");

}


/***********************************************************************
 * Purpose:             for parsing output list
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <output_list> ->
 *                      	<opt_variable_list> | STR_T
 * First set:           FIRST(<output_list>) = {AVID_T, SVID_T, STR_T, ϵ, STR_T} ***should we fix the remove STR_T? check grammar too!***
 **********************************************************************/
void output_list(void) {

    /* if not optional variable, then epsilon */
    switch(lookahead.code) {

        case AVID_T:
        case SVID_T:
        case STR_T:
            variable_list();
            break;

    }

    gen_incode("PLATY: Output list parsed");

}


/***********************************************************************
 * Purpose:             for parsing optional variable list
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <opt_variable_list> ->
 *                      	<variable_list> | ϵ
 * First set:           FIRST(<opt_variable_list>) = {AVID_T, SVID_T, STR_T, ϵ}
 **********************************************************************/
void opt_variable_list(void) {

    /* if not optional variable, then epsilon */
    switch(lookahead.code) {

        case AVID_T:
        case SVID_T:
        case STR_T:
            variable_list();
            break;

    }

    gen_incode("PLATY: Optional variable list parsed");

}


/***********************************************************************
 * Purpose:             for parsing arithmetic expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <arithmetic_expression> - >
 *                      	<unary_arithmetic_expression> | <additive_arithmetic_expression>
 * First set:           FIRST(<arithmetic_expression>) = {-, +, AVID_T, FPL_T, INL_T, (}
 **********************************************************************/
void arithmetic_expression(void) {

    switch(lookahead.code) {

        case ART_OP_T:
            switch(lookahead.attribute.arr_op) {

                case MINUS:
                case PLUS:
                    unary_arithmetic_expression();
                    break;

                /* print error on anything else */
                default:
                    syn_printe();
                    return;

            }

        case AVID_T:
        case FPL_T:
        case INL_T:
        case LPR_T:
            additive_arithmetic_expression();
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Arithmetic expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing unary arithmetic expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <unary_arithmetic_expression> ->
 *                      	- <primary_arithmetic_expression> |	+ <primary_arithmetic_expression>
 * First set:           FIRST(<unary_arithmetic_expression>) = {-, +}
 **********************************************************************/
void unary_arithmetic_expression(void) {

    switch(lookahead.code) {

        case MINUS:
        case PLUS:
            match(lookahead.code, lookahead.attribute.arr_op);
            primary_arithmetic_expression();
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Unary arithmetic expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing additive arithmetic expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <additive_arithmetic_expression> ->
 *                      	<multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime>
 * First set:           FIRST(<additive_arithmetic_expression>) = {AVID_T, FPL_T, INL_T, (}
 **********************************************************************/
void additive_arithmetic_expression(void) {

    multiplicative_arithmetic_expression();
    additive_arithmetic_expression_prime();

    gen_incode("PLATY: Additive arithmetic expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing additive arithmetic expression prime
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <additive_arithmetic_expression_prime> ->
 *                      	+ <multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime> |
 *                      	- <multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime> |
 *                      	ϵ
 * First set:           FIRST(<additive_arithmetic_expression_prime>) = {+, -, ϵ}
 **********************************************************************/
void additive_arithmetic_expression_prime(void) {

    /* test for arithmetic opperator, else epsilon */
    if (lookahead.code == ART_OP_T) {

        switch(lookahead.attribute.arr_op) {

            case PLUS:
            case MINUS:
                match(lookahead.code, lookahead.attribute.arr_op);
                multiplicative_arithmetic_expression();
                additive_arithmetic_expression_prime();

                gen_incode("PLATY: Additive arithmetic expression prime parsed");

            /* print error on anything else */
            default:
                syn_printe();
                return;

        }

    }

}


/***********************************************************************
 * Purpose:             for parsing multiplicative arithmetic expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <multiplicative_arithmetic_expression> ->
 *                      	<primary_arithmetic_expression> <multiplicative_arithmetic_expression_prime>
 * First set:           FIRST(<multiplicative_arithmetic_expression>) = {AVID_T, FPL_T, INL_T, (}
 **********************************************************************/
void multiplicative_arithmetic_expression(void) {

    primary_arithmetic_expression();
    multiplicative_arithmetic_expression_prime();

    gen_incode("PLATY: Multiplicative arithmetic expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing multiplicative arithmetic expression_prime
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <multiplicative_arithmetic_expression_prime> ->
 *                      	* <primary_arithmetic_expression> <multiplicative_arithmetic_expression_prime> |
 *                      	/ <primary_arithmetic_expression> <multiplicative_arithmetic_expression_prime> |
 *                      	ϵ
 * First set:           FIRST(<multiplicative_arithmetic_expression_prime>) = {*, /, ϵ}
 **********************************************************************/
void multiplicative_arithmetic_expression_prime(void) {

    /* test for arithmetic opperator, else epsilon */
    if (lookahead.code == ART_OP_T) {

        switch(lookahead.attribute.arr_op) {

            case MULT:
            case DIV:
                match(lookahead.code, lookahead.attribute.arr_op);
                primary_arithmetic_expression();
                multiplicative_arithmetic_expression_prime();

                gen_incode("PLATY: Multiplicative arithmetic expression prime parsed");

            /* print error on anything else */
            default:
                syn_printe();
                return;

        }

    }

}


/***********************************************************************
 * Purpose:             for parsing primary arithmetic expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <primary_arithmetic_expression> ->
 *                      	AVID_T | FPL_T | INL_T | ( <arithmetic_expression> )
 * First set:           FIRST(<primary_arithmetic_expression>) = {AVID_T, FPL_T, INL_T, (}
 **********************************************************************/
void primary_arithmetic_expression(void) {

    switch(lookahead.code) {

        case AVID_T:
        case FPL_T:
        case INL_T:
            match(lookahead.code, lookahead.attribute.arr_op);
            break;

        case LPR_T:
            match(lookahead.code, lookahead.attribute.arr_op);
            arithmetic_expression();
            match(RPR_T, NO_ATTR);
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Primary arithmetic expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing string expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <string_expression> ->
 *                      	<primary_string_expression> <string_expression_prime>
 * First set:           FIRST(<string_expression>) = {SVID_T, STR_T}
 **********************************************************************/
void string_expression(void) {

    primary_string_expression();
    string_expression_prime();

    gen_incode("PLATY: String expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing string expression prime
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <string_expression_prime> ->
 *                      	# <primary_string_expression> <string_expression_prime> | ϵ
 * First set:           FIRST(<string_expression_prime>) = {#, ϵ}
 **********************************************************************/
void string_expression_prime(void) {

    /* test for string concatination opperator, else epsilon */
    if(lookahead.code == SCC_OP_T) {

        match(lookahead.code, lookahead.attribute.arr_op);
        primary_string_expression();
        string_expression_prime();

    }

    /* print string parsed even if string is empty */
    gen_incode("PLATY: String expression prime parsed");

}


/***********************************************************************
 * Purpose:             for parsing primary string expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <primary_string_expression> ->
 *                      	SVID_T | STR_T
 * First set:           FIRST(<primary_string_expression>) = {SVID_T, STR_T}
 **********************************************************************/
void primary_string_expression(void) {

    switch(lookahead.code) {

        case SVID_T:
        case STR_T:
            match(lookahead.code, lookahead.attribute.arr_op);
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Primary string expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing conditional expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <conditional_expression> ->
 *                      	<logical_OR_expression>
 * First set:           FIRST(<conditional_expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 **********************************************************************/
void conditional_expression(void) {

    logical_OR_expression();

    gen_incode("PLATY: Conditional expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing logical OR expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <logical_OR_expression> ->
 *                      	<logical_AND_expression> <logical_OR_expression_prime>
 * First set:           FIRST(<logical_OR_expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 **********************************************************************/
void logical_OR_expression(void) {

    logical_AND_expression();
    logical_OR_expression_prime();

    gen_incode("PLATY: Logical OR expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing logical OR expression prime
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <logical_OR_expression_prime> ->
 *                      	.OR. <logical_AND_expression> <logical_OR_expression_prime> | ϵ
 * First set:           FIRST(<logical_OR_expression_prime>) = {.OR., ϵ}
 **********************************************************************/
void logical_OR_expression_prime(void) {

    /* test for logical operator token, do nothing if epsilon */
    if (lookahead.code == LOG_OP_T) {

        switch(lookahead.attribute.log_op) {

            case OR:
                match(lookahead.code, lookahead.attribute.arr_op);
                logical_AND_expression();
                logical_OR_expression_prime();
                break;

            /* print error on anything else */
            default:
                syn_printe();
                return;

        }

        gen_incode("PLATY: Logical OR expression prime parsed");

    }

}


/***********************************************************************
 * Purpose:             for parsing logical AND expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <logical_AND_expression> ->
 *                      	<relational_expression> <logical_AND_expression_prime>
 * First set:           FIRST(<logical_AND_expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 **********************************************************************/
void logical_AND_expression(void) {

    relational_expression();
    logical_AND_expression_prime();

    gen_incode("PLATY: Logical AND expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing logical AND expression prime
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <logical_AND_expression_prime> ->
 *                      	.AND. <relational_expression> <logical_AND_expression_prime> | ϵ
 * First set:           FIRST(<logical_AND_expression prime>) = {.AND., ϵ}
 **********************************************************************/
void logical_AND_expression_prime(void) {

    /* test for logical operator token, do nothing if epsilon */
    if (lookahead.code == LOG_OP_T) {

        switch(lookahead.attribute.log_op) {

            case AND:
                match(lookahead.code, lookahead.attribute.arr_op);
                relational_expression();
                logical_AND_expression();
                break;

            /* print error on anything else */
            default:
                syn_printe();
                return;

        }

        gen_incode("PLATY: Logical AND expression prime parsed");

    }

}


/***********************************************************************
 * Purpose:             for parsing relational expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <relational_expression> ->
 *                      	<primary_a_relational_expression> <primary_a_relational_expression_list> |
 *                      	<primary_s_relational_expression> <primary_s_relational_expression_list>
 * First set:           FIRST(<relational_expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 **********************************************************************/
void relational_expression(void) {

    switch(lookahead.code) {

        case AVID_T:
        case FPL_T:
        case INL_T:
            primary_a_relational_expression();
            primary_a_relational_expression_list();
            break;

        case SVID_T:
        case STR_T:
            primary_s_relational_expression();
            primary_s_relational_expression_list();
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Relational expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing primary a relational expression list
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <primary_a_relational_expression_list> ->
 *                      	==  <primary_a_relational_expression> |
 *                      	<>  <primary_a_relational_expression> |
 *                      	>   <primary_a_relational_expression> |
 *                      	<   <primary_a_relational_expression>
 * First set:           FIRST(<primary_a_relational_expression list>) = {==, <>, >, <}
 **********************************************************************/
void primary_a_relational_expression_list(void) {

    switch(lookahead.attribute.rel_op) {

        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead.code, lookahead.attribute.arr_op);
            primary_a_relational_expression();
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Primary a relational expression list parsed");

}


/***********************************************************************
 * Purpose:             for parsing primary s relational expression list
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <primary_s_relational_expression_list> ->
 *                      	==  <primary_s_relational_expression> |
 *                      	<>  <primary_s_relational_expression> |
 *                      	>   <primary_s_relational_expression> |
 *                      	<   <primary_s_relational_expression>
 * First set:           FIRST(<primary_s_relational_expression list>) = {==, <>, >, <}
 **********************************************************************/
void primary_s_relational_expression_list(void) {

    switch(lookahead.attribute.rel_op) {

        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead.code, lookahead.attribute.arr_op);
            primary_s_relational_expression();
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Primary s relational expression list parsed");

}


/***********************************************************************
 * Purpose:             for parsing primary a relational expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <primary_a_relational_expression> ->
 *                      	AVID_T | FPL_T | INL_T
 * First set:           FIRST(<primary_a_relational_expression>) = {AVID_T, FPL_T, INL_T}
 **********************************************************************/
void primary_a_relational_expression(void) {

    switch(lookahead.code) {

        case AVID_T:
        case FPL_T:
        case INL_T:
            match(lookahead.code, lookahead.attribute.rel_op);
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Primary a relational expression parsed");

}


/***********************************************************************
 * Purpose:             for parsing primary s relational expression
 * Author:              Cory Hilliard    040 630 141
 *                      Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          none
 * Return value:        none
 * Production:          <primary_s_relational_expression> ->
 *                      	<primary_string_expression>
 * First set:           FIRST(<primary_s_relational_expression>) = {SVID_T, STR_T}
 **********************************************************************/
void primary_s_relational_expression(void) {

    switch(lookahead.code) {

        case SVID_T:
        case STR_T:
            primary_string_expression();
            break;

        /* print error on anything else */
        default:
            syn_printe();
            return;

    }

    gen_incode("PLATY: Primary s relational expression parsed");

}


/*
Step 8:
    Build your parser incrementally, function by function. After adding a function, test the
    parser thoroughly. Use your main program (modify one of the previous main programs)
    to test the parser. The official main program and the test set for this assignment will be
    provided in two weeks.
*/
