/***********************************************************************
 * File name:       scanner.c
 * Compiler:        gcc
 * IDE:             Code::Blocks version 13.12 on (mixed environments)
 * Authors:         Cory Hilliard    040 630 141 (Linux - Fedora 23)
 *                  Matthew Clements 040 766 220 (Windows - v8)
 * Course:          CST8152 - Compilers, Lab Section: 011
 * Assignment:      02 Lexical Analyzer (Scanner)
 * Due Date:        Tuesday October 27 2015
 * Professor:       Svillen Ranev
 * Purpose:         Functions implementing a Lexical Analyzer (Scanner)
 * Function list:   aa_func02()
 *                  aa_func03()
 *                  aa_func05()
 *                  aa_func08()
 *                  aa_func11()
 *                  aa_func12()
 *                  aa_func13()
 *                  atool()
 *                  char_class()
 *                  get_next_state()
 *                  iskeyword()
 *                  malpar_next_token()
 *                  scanner_init()
 **********************************************************************/
/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>          /* standard input / output */
#include <ctype.h>          /* conversion functions */
#include <stdlib.h>         /* standard library functions and constants */
#include <string.h>         /* string functions */
#include <limits.h>         /* integer types constants */
#include <float.h>          /* floating-point types constants */

/*#define NDEBUG*/          /* to suppress assert() call */
#include <assert.h>         /* assert() prototype */

#define INIT_CAPACITY 200   /* initial buffer capacity */
#define INC_FACTOR    15    /* increment factor */
#define P_USHRT_MAX   65535 /* constant two-byte unsigned max */

#define DEBUG               /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/*
   This buffer is used as a repository for string literals.
   It is defined in platy_st.c
*/
extern Buffer* str_LTBL;                        /* string literal table */
int line;                                       /* current line number of the source code */
extern int scerrnum;                            /* defined in platy_st.c - run-time error number */
extern STD sym_table;                           /* defined in platy_tt.c */

/* Local(file) global objects - variables */
static Buffer *lex_buf;                         /*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c);                  /* character class function */
static int get_next_state(int, char, int *);    /* state machine function */
static long atool(char * lexeme);               /* converts octal string to decimal value */

int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;  /* 1 */
	/* in case the buffer has been read previously */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  */                          /* no need - global ANSI C */
}


/***********************************************************************
 * Purpose:             Reads in a lexeme character by character from the buffer and tries to match it to an appropriate token.
 * Author:              Cory Hilliard       040 630 141
 *                      Matthew Clements    040 766 220
 * History/Versions:    1.0
 * Called functions:    b_getc()
 *                      b_size()
 *                      b_getc_offset()
 *                      b_setmark()
 *                      b_getc_offset()
 *                      b_retract_to_mark()
 *                      b_retract()
 *                      b_addc()
 *                      get_next_state()
 *                      b_create()
 *                      aa_table[state]()
 *                      b_destroy()
 * Parameters:          sc_buf: Buffer*
 * Return value:        tempBuffer
 * Algorithm:           create buffer and char string in dynamic memory
 *                      set the mode, capacity and inc_factor according to the mode and inc_factor
 *                      return tempBuffer
 ** Important note:     It is the responsibility of the calling function to test for null buffer**
 **********************************************************************/
Token mlwpar_next_token(Buffer* sc_buf){
    Token t;             /* token to return after recognition */
    unsigned char c;     /* input symbol */
    int state = 0;       /* initial state of the FSM */
    short lexstart;      /* start offset of a lexeme in the input buffer */
    short lexend;        /* end offset of a lexeme in the input buffer */
    short lexlength;     /* holds length of lexeme */
    int accept = NOAS;   /* type of state - initially not accepting */
    int i;               /* for itteration */

    /* responsibility of the calling function to test for null buffer */
    /*
        if (sc_buf == NULL){
            t.code = ERR_T;
            strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
            scerrnum = 1;
            return t;
        }
    */

    /* single-lexeme tokens processed separately one by one */
    while(1) {  /* endless loop broken by token returns it will generate a warning */

        /* get the next symbol from the input buffer */
        c = b_getc(sc_buf);

        /* special cases or token driven processing */
        switch(c)
        {

            /* end of buffer characters */
            case SEOF1: /* '\0' */
            case SEOF2: /* 0xFF */
                        t.code = SEOF_T;
                        return t;

            /* white spaces characters */
            case ' ':   /* space */
            case '\t':  /* horizontal tab */
			case '\v':  /* vertical tab */
			case '\f':  /* form feed */

			/* line terminating characters */
            case '\r':  /* carriage return */
                        continue;
            /* TODO: Discuss if we need this */
            /*
            case '\r':
				c = b_getc(sc_buf);
				if( c != '\n' ){
					line++;
				}
				b_retract(sc_buf);
                break;
            */

            case '\n':  /* new line characters need to be counted */
                        line++;
                        continue;

            /* comments */
            case '!':   /* exclamation */

                        /*
                            Error handling in comments. If the comment construct is not lexically (as defined in
                            the grammar) correct, the scanner must return an error token. For example, if the
                            scanner finds only a symbol ! not followed by < it returns an error token. The C-type
                            string attribute of the comment error token is the ! and the first symbol following !.
                        */

                        /* get the next symbol from the input buffer */
                        c = b_getc(sc_buf);

                        if (c != '<') {

                            t.code = ERR_T;
                            t.attribute.err_lex[0] = '!';
                            t.attribute.err_lex[1] = c;
                            t.attribute.err_lex[2] = '\0';
                            /* loop through characters until new line */
                            while(c != '\n') {
                                c = b_getc(sc_buf);
                            }
                            line++;
                            return t;

                        }

                        /* if next char is a '<' loop until end of line */
                        while(c != '\n') {
                            c = b_getc(sc_buf);
                        }
                        line++;
                        continue;

            /* string */
            case '"':
                    {
                        /*
                            Error handling in strings. In the case of illegal strings, the scanner must return an
                            error token. The erroneous string must be stored as a C-type string in the attribute
                            part of the error token (not in the string literal table). If the erroneous string is
                            longer than 20 characters, you must store the first 17 characters only and then
                            append three dots (...) at the end.
                        */

                        short lexlength;    /* holds lexeme length */
                        short oldoffset;    /* holds old offset of str_LBTL */
                        int i;              /* for itteration */

                        /* get addc_offset of str_LTBL */
                        oldoffset = b_size(str_LTBL);

                        /* set lexstart */
                        lexstart = b_getc_offset(sc_buf);

                        /* set mark to lexstart */
                        b_setmark(sc_buf, lexstart);

                        /* get next char to start the loop */
                        c = b_getc(sc_buf);

                        /* loop until next quotation mark */
                        while (c != '"' && c != SEOF1 && c != SEOF2)
                        {

                            /* test for new line and increment */
                            if (c == '\n') {

                                ++line;

                            }

                            c = b_getc(sc_buf);

                        }

                        /* test for SEOF */
                        if (c == SEOF1 || c == SEOF2)
                        {

                            /* set error code */
                            t.code = ERR_T;

                            /* set the lexend */
                            lexend = b_getc_offset(sc_buf);

                            /* set length of the lexeme */
                            lexlength = (lexend - lexstart);

                            /* retract to mark */
                            b_retract_to_mark (sc_buf);
                            b_retract(sc_buf);

                            /* reset counter */
                            i = 0;

                            /* copy string into err_lex */
                            while(i < lexlength && i < ERR_LEN)
                            {

                                /* get next char from the buffer */
                                c = b_getc(sc_buf);

                                if (lexlength <= ERR_LEN)
                                {
                                    t.attribute.err_lex[i] = c;
                                }

                                else
                                {
                                    /* while i is less than ERR_LEN - 3, add c */
                                    if (i < ERR_LEN - 3)
                                    {
                                        t.attribute.err_lex[i] = c;
                                    }

                                    else
                                    {
                                        t.attribute.err_lex[i] = '.';
                                    }
                                }

                                i++;

                            }

                            /* add the trailing '\0' */
                            t.attribute.err_lex[i] = '\0';

                            /* set the buffer back to the end of the input */
                            b_setmark(sc_buf, lexend);

                            while (c != SEOF1 && c != SEOF2){
                                c = b_getc(sc_buf);
                            }

                            return t;
                        }

                        /* string is valid, copy string to str_LTBL */
                        b_retract(sc_buf);

                        /* set lexend */
                        lexend = b_getc_offset(sc_buf);

                        /* set lexlength */
                        lexlength = (lexend - lexstart);

                        /* retract mark */
                        b_retract_to_mark(sc_buf);

                        /* reset counter */
                        i = 0;

                        while(i < lexlength)
                        {

                            /* get next char from the buffer */
                            c = b_getc(sc_buf);

                            /* add char to the str_LTBL */
                            b_addc(str_LTBL, c);

                            i++;

                        }

                        /* add '\0' to str_LTBL */
                        b_addc(str_LTBL, '\0');

                        /* consume the last quotation */
                        c = b_getc(sc_buf);

                        /* set token values */
                        t.code = STR_T;
                        t.attribute.str_offset = oldoffset;

                        return t;

                    }

            /* aritmetic operators */
            case '+':   /* plus = 0 */
                        t.code = ART_OP_T;
                        t.attribute.arr_op = PLUS;
                        return t;
            case '-':   /* minus = 1 */
                        t.code = ART_OP_T;
                        t.attribute.arr_op = MINUS;
                        return t;
            case '*':   /* astrisk = 2 */
                        t.code = ART_OP_T;
                        t.attribute.arr_op = MULT;
                        return t;
            case '/':   /* slash = 3 */
                        t.code = ART_OP_T;
                        t.attribute.arr_op = DIV;
                        return t;

            /* string operator */
            case '#':
                        t.code = SCC_OP_T;
                        return t;

            /* parenthases */
            case '(':   /* left parenthases */
                        t.code = LPR_T;
                        /*no attribute */
                        return t;
            case ')':   /* right parenthases */
                        t.code = RPR_T;
                        /*no attribute */
                        return t;
            case '{':   /* left brace */
                        t.code = LBR_T;
                        /*no attribute */
                        return t;
            case '}':   /* right brace */
                        t.code = RBR_T;
                        /*no attribute */
                        return t;

            /* comma */
            case ',':   /* comma */
                        t.code = COM_T;
                        /* no attribute */
                        return t;

            /* semicolon */
            case ';':   /* semicolon/end of statement */
                        t.code = EOS_T;
                        /* no attribute */
                        return t;

            /* logical operators .AND. & .OR. */
            case '.':   /* period */
                        b_setmark(sc_buf, b_getc_offset(sc_buf));
                        c = b_getc(sc_buf);

                        /* try to process .AND. */
                        if (c == 'A') {
                            c = b_getc(sc_buf);
                            if (c == 'N') {
                                c = b_getc(sc_buf);
                                if (c == 'D') {
                                    c = b_getc(sc_buf);
                                    if (c == '.') {
                                        t.code = LOG_OP_T;
                                        t.attribute.log_op = AND;
                                        return t;
                                    }


                                }

                            }


                        }

                        /* try to process .OR. */
                        else if (c == 'O') {
                            c = b_getc(sc_buf);
                            if (c == 'R') {
                                c = b_getc(sc_buf);
                                if (c == '.') {
                                    t.code = LOG_OP_T;
                                    t.attribute.log_op = OR;
                                    return t;
                                }

                            }

                        }

                        /* if .AND. and .OR. failed, return error token */
                        b_retract_to_mark(sc_buf);
                        t.code = ERR_T;
                        t.attribute.err_lex[0] = '.';
                        t.attribute.err_lex[1] = '\0';
                        return t;


            /* assignment and equality */
            case '=':   /* equals */

                        c = b_getc(sc_buf);

                        if (c == '=') {
                            t.code = REL_OP_T;
                            t.attribute.rel_op = EQ;
                            return t;
                        }

                        /* c is some other symbol so t.code is '=' */
                        t.code = ASS_OP_T;
                        /* no attribute */

                        /* retract the buffer */
                        b_retract(sc_buf);
                        return t;

            /* relational operators */
            case '<':   /* less than */

                        c = b_getc(sc_buf);

                        /* not equals */
                        if (c == '>') {
                            t.code = REL_OP_T;
                            t.attribute.rel_op = NE;
                            return t;
                        }

                        /* c is some other symbol than > t.code is '<' (less than) */
                        t.code = REL_OP_T;
                        t.attribute.rel_op = LT;

                        /* retract the buffer */
                        b_retract(sc_buf);
                        return t;


            case '>':   /* greater than */
                        t.code = REL_OP_T;
                        t.attribute.rel_op = GT;
                        return t;


        } /* end switch(c) */

        /* process state transition table */
        if (isalnum(c))/*  if (c is a digit OR c is a letter){*/
        {

            /*  set mark at the beginning of the lexeme */
            b_setmark(sc_buf,b_getc_offset(sc_buf) - 1);

            /* begin with state = 0 and the input character c */

                /* get the next state from the transition table calling */
                state = get_next_state(state, c, &accept);

                /* while the state is not accepting (NOAS), get next char/state */
                while (accept == NOAS)
                {

                    /* get the next character */
                    c = b_getc(sc_buf);

                    /* get next state */
                    state = get_next_state(state, c, &accept);

                }

                /* retract getc_offset if the final state is a retracting final state  */
                if (accept == ASWR){

                    b_retract(sc_buf);

                }

                /* set lexstart to the beginning of the lexeme */
                lexstart = b_mark(sc_buf);

                /* set lexend to the end of the lexeme */
                lexend = b_getc_offset(sc_buf);

                /* get lexeme length */
                lexlength = (lexend - lexstart);

                /* create temporary lexeme buffer */
                lex_buf = b_create(INIT_CAPACITY,INC_FACTOR,'a');

                /* test if b_create returned NULL */
	            if (lex_buf == NULL){
                    t.code = ERR_T;
                    strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
                    scerrnum = 1;
                    return t;
	            }

                /* RETRACT getc_offset to the MARK SET PREVIOUSLY AT THE BEGINNING OF THE LEXEME */
                b_retract_to_mark(sc_buf);

                /* USING b_getc() COPY THE LEXEME BETWEEN */
                /* lexstart AND lexend FROM THE INPUT BUFFER INTO lex_buf USING b_addc(...), */
                for(i = 0; i < lexlength; i++){
                    b_addc(lex_buf, b_getc(sc_buf));
                }

                b_addc(lex_buf, '\0');

                /*
                    WHEN VID (KEYWORDS INCLUDED), FPL OR IL IS RECOGNIZED
                    YOU MUST CALL THE ACCEPTING FUNCTION USING THE ARRAY aa_table ,WHICH
                    CONTAINS POINTERS TO FUNCTIONS. THE ARRAY INDEX OF THE FUNCTION TO BE
                    CALLED IS STORED IN THE VARIABLE state.
                    THE ARGUMENT TO THE FUNCTION IS THE STRING STORED IN lex_buf.
                */

                t = aa_table[state](b_setmark(lex_buf, 0));
                b_destroy(lex_buf);
                return t;
        }

        /*
            CHECK OTHER CHARS HERE if NEEDED, SET A TOKEN AND RETURN IT.
            FOR ILLEGAL CHARACTERS SET ERROR TOKEN.
            THE ILLEGAL CHAR IS THE ATTRIBUTE OF THE ERROR TOKEN
        */

        /* catches all remaining error symbols */
        t.code = ERR_T;
        t.attribute.err_lex[0] = c;
        t.attribute.err_lex[1] = '\0';
        return t;

    } /* end while(1) */

} /* end mlwpar_next_token(Buffer * sc_buf) */


/*********************************************
 * DO NOT MODIFY THE CODE OF THIS FUNCTION YOU CAN REMOVE THE COMMENTS
 * Author:              Svillen Ranev
**********************************************/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
    /*
        The assert(int test) macro can be used to add run-time diagnostic to programs
        and to "defend" from producing unexpected results.
        assert() is a macro that expands to an if statement;
        if test evaluates to false (zero) , assert aborts the program
        (by calling abort()) and sends the following message on stderr:

        Assertion failed: test, file filename, line linenum

        The filename and linenum listed in the message are the source file name
        and line number where the assert macro appears.
        If you place the #define NDEBUG directive ("no debugging")
        in the source code before the #include <assert.h> directive,
        the effect is to comment out the assert statement.
    */
    assert(next != IS);

    /*
        The other way to include diagnostics in a program is to use
        conditional preprocessing as shown bellow. It allows the programmer
        to send more details describing the run-time problem.
        Once the program is tested thoroughly #define DEBUG is commented out
        or #undef DEBUF is used - see the top of the file.
    */
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}


/***********************************************************************
 * Purpose:             THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION TABLE: "st_table"
 * Author:              Cory Hilliard 040 630 141
 * History/Versions:    1.0
 * Called functions:    isalpha()
 * Parameters:          char
 * Return value:        tempBuffer
 * Algorithm:           TABLE st_table FOR THE INPUT CHARACTER c.
 *                      SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
 *                      FOR EXAMPLE IF COLUMN 1 REPRESENTS [A-Z]
 *                      THE FUNCTION RETURNS 1 EVERY TIME c IS ONE
 *                      OF THE LETTERS A,B,...,Z.
 **********************************************************************/
int char_class (char c)
{
    /* test for alpha chars */
    if(isalpha(c)){
        return 0;
    }

    /* test for numerical chars */
    if( c == '0' ){
        return 1;
    }

    if( c >= '1' && c <= '7' ){
        return 2;
    }

    if( c == '8' || c == '9' ){
        return 3;
    }

    /* test for '.' and '%' chars */
    if( c == '.' ){
        return 4;
    }

    if( c == '%' ){
        return 5;
    }

    /* everything else */
    return 6;

}


/***********************************************************************
 * Purpose:             ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    strcmp()
 *                      strlen()
 * Parameters:          char array [ ]  lexeme
 * Return value:        tempBuffer
 * Algorithm:           WHEN CALLED THE FUNCTION MUST
 *                      1. CHECK IF THE LEXEME IS A KEYWORD.
 *                         IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
 *                         FOR THE KEYWORD. THE ATTRIBUTE CODE FOR THE KEYWORD
 *                         IS ITS INDEX IN THE KEYWORD LOOKUP TABLE (kw_table in table.h).
 *                         IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.
 *                      2. SET a AVID TOKEN.
 *                         IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
 *                         ONLY FIRST VID_LEN CHARACTERS ARE STORED
 *                         INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
 *                         ADD \0 AT THE END TO MAKE A C-type STRING.
 **********************************************************************/
Token aa_func02(char lexeme[]){

    Token t;            /* create token to pass to calling function */
    int x;              /* used for index counting through the keyword table */
    int string_length;  /* used to store the length of the lexeme[] parameter */
    char vid_lex[VID_LEN+1] = {0};

    for( x = 0; x < KWT_SIZE; x++){
        if(!strcmp(kw_table[x], lexeme)){
            t.code = KW_T;
            t.attribute.kwt_idx = x;
            return t;
        }
    }

    string_length = strlen(lexeme);

    t.code = AVID_T;

    for ( x = 0; x < VID_LEN; x++ ){

        if(x < string_length){

            vid_lex[x] = lexeme[x];
        }

    }

    if ( string_length < VID_LEN ) {

        vid_lex[string_length] = '\0';

    }

    else {

        vid_lex[VID_LEN] = '\0';

    }

    if ( ( vid_lex[0] == 'i' || vid_lex[0] == 'o' || vid_lex[0] == 'd' || vid_lex[0] == 'w' ) ){

        t.attribute.vid_offset = st_install(sym_table, vid_lex, 'I', line);

        if (t.attribute.vid_offset == -1) {

            printf("\nError: The Symbol Table is full - install failed.\n");
            st_store(sym_table);
            b_destroy(lex_buf);
            exit(1);

        }

    }

    else {

        t.attribute.vid_offset = st_install(sym_table, vid_lex, 'F', line);

        if (t.attribute.vid_offset == -1) {

            printf("\nError: The Symbol Table is full - install failed.\n");
            st_store(sym_table);
            b_destroy(lex_buf);
            exit(1);

        }
    }

    return t;
}


/***********************************************************************
 * Purpose:             ACCEPTING FUNCTION FOR THE string variable identifier (VID - SVID)
 * Author:              Cory Hilliard 040 630 141
 * History/Versions:    1.0
 * Called functions:    strlen()
 * Parameters:          char array[] lexeme
 * Return value:        Token t
 * Algorithm:           WHEN CALLED THE FUNCTION MUST
 *                      1. SET a SVID TOKEN.
 *                      IF THE lexeme IS LONGER than VID_LEN characters,
 *                      ONLY FIRST VID_LEN-1 CHARACTERS ARE STORED
 *                      INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
 *                      AND THEN THE % CHARACTER IS APPENDED TO THE NAME.
 *                      ADD \0 AT THE END TO MAKE A C-type STRING
 **********************************************************************/
Token aa_func03(char lexeme[]){

    Token t;                /* create token to pass to calling function */
    int i;                  /* for itteration */
    int lexemeLength = 0;   /* for holding size of lexeme length */
    char vid_lex[VID_LEN+1] = {0};

    /* set SVID token */
    t.code = SVID_T;

    /* get string length */
    lexemeLength = strlen(lexeme);

    /* store variable ID into vid_lex */
    for( i = 0; i < VID_LEN-1; i++ ){
        if(i < lexemeLength){
            vid_lex[i] = lexeme[i];
        }
    }

    if(lexemeLength <= VID_LEN-1){
        if(lexeme[(lexemeLength - 1)] != '%'){
            vid_lex[(lexemeLength - 1)] = '%';
        }
        vid_lex[lexemeLength] = '\0';
    }

    else{
        if(lexeme[lexemeLength] != '%'){
            vid_lex[VID_LEN-1] = '%';
        }
        vid_lex[VID_LEN] = '\0';
    }

    t.attribute.vid_offset = st_install(sym_table, vid_lex, 'S', line);

    if (t.attribute.vid_offset == -1) {

        printf("\nError: The Symbol Table is full - install failed.\n");
        st_store(sym_table);
        b_destroy(lex_buf);
        exit(1);

    }


    /* return token */
    return t;
}


/***********************************************************************
 * Purpose:             ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL) AND ZERO (0)
 * Author:              Cory Hilliard 040 630 141
 * History/Versions:    1.0
 * Called functions:    atol()
 * Parameters:          char array[] lexeme
 * Return value:        Token t
 * Algorithm:           THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT AND 0
 *                      TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
 *                      THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte int in C.
 *                      IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
 *                      THE ERROR TOKEN ATTRIBUTE IS  lexeme
 **********************************************************************/
Token aa_func05(char lexeme[]){

    Token t;                    /* create token to pass to calling function */
    unsigned long decimalValue; /* used to hold decimal value from conversion function */
    int i;                      /* for iteration */

    decimalValue = atol(lexeme);

    if (decimalValue >= P_USHRT_MAX){

        /* return error token on out of range */
        t.code = ERR_T;

        /* store variable ID into vid_lex */
        for( i = 0; i < ERR_LEN; i++ ){
            t.attribute.err_lex[i] = lexeme[i];
        }

        t.attribute.err_lex[i] = '\0';

        return t;

    }

    t.code = INL_T;
    t.attribute.int_value = decimalValue;

    return t;
}


/***********************************************************************
 * Purpose:             ACCEPTING FUNCTION FOR THE floating-point literal (FPL)
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    strtod()
 * Parameters:          char array [ ] lexeme
 * Return value:        tempBuffer
 * Algorithm:           THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
 *                      WHICH IS THE ATTRIBUTE FOR THE TOKEN.
 *                      THE VALUE MUST BE IN THE SAME RANGE AS the value of 4-byte float in C.
 *                      IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
 *                      THE ERROR TOKEN ATTRIBUTE IS  lexeme
 **********************************************************************/
Token aa_func08(char lexeme[]){

    Token t;            /* create token to pass to calling function */
    double floatValue;  /* used to hold float value from conversion function */
    int i;              /* for iteration */

    floatValue = strtod(lexeme, NULL);
    if (floatValue >= FLT_MAX || (floatValue < FLT_MIN && floatValue != 0.0f)){

        /* return error token on out of range */
        t.code = ERR_T;

        /* store variable ID into vid_lex */
        for( i = 0; i < ERR_LEN; i++ ){
            t.attribute.err_lex[i] = lexeme[i];
        }

        t.attribute.err_lex[i] = '\0';

        return t;
    }

    t.code = FPL_T;
    t.attribute.flt_value = (float)floatValue;

    return t;
}


/***********************************************************************
 * Purpose:             ACCEPTING FUNCTION FOR THE integer literal(IL) - octal constant (OIL)
 * Author:              Cory Hilliard 040 630 141
 * History/Versions:    1.0
 * Called functions:    atool()
 * Parameters:          char array [ ] lexeme
 * Return value:        tempBuffer
 * Algorithm:           THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING AN OCTAL CONSTANT
 *                      TO A DECIMAL INTEGER VALUE WHICH IS THE ATTRIBUTE FOR THE TOKEN.
 *                      THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte int in C.
 *                      THIS FUNCTION IS SIMILAR TO THE FUNCTION ABOVE AND THEY CAN BE
 *                      COMBINED INTO ONE FUNCTION
 *                      THE MAIN DIFFERENCE IE THAT THIS FUNCTION CALLS
 *                      THE FUNCTION atool(char * lexeme) WHICH CONVERTS AN ASCII STRING
 *                      REPRESENTING AN OCTAL NUMBER TO INTEGER VALUE
 *                      IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
 *                      THE ERROR TOKEN ATTRIBUTE IS  lexeme
 **********************************************************************/
Token aa_func10(char lexeme[]){

    Token t;            /* create token to pass to calling function */
    long octalValue;    /* used to hold octal value from conversion function */
    int i;              /* for iteration */

    /* convert lexeme into an octal literal */
    octalValue = atool(lexeme);

    if (octalValue >= P_USHRT_MAX){

        /* return error token on out of range */
        t.code = ERR_T;
        /* store variable ID into vid_lex */
        for( i = 0; i < ERR_LEN; i++ ){
            t.attribute.err_lex[i] = lexeme[i];
        }

        t.attribute.err_lex[i] = '\0';

        return t;

    }

    t.code = INL_T;
    t.attribute.int_value = octalValue;

    return t;
}


/***********************************************************************
 * Purpose:             ACCEPTING FUNCTION FOR THE ERROR TOKEN NO RETRACT
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    strlen()
 * Parameters:          char array [ ] lexeme
 * Return value:        tempBuffer
 * Algorithm:           THE FUNCTION SETS THE ERROR TOKEN. lexeme[] CONTAINS THE ERROR
 *                      THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme ITSELF
 *                      AND IT MUST BE STORED in err_lex.  IF THE ERROR LEXEME IS LONGER
 *                      than ERR_LEN caharacters, only the first ERR_LEN character are
 *                      stored in err_lex.
 **********************************************************************/
Token aa_func12(char lexeme[]){

    Token t;            /* create token to pass to calling function */
    int x;              /* for itteration */
    int string_length;  /* for holding size of lexeme length */

    /* get string length */
    string_length = strlen(lexeme);

    t.code = ERR_T;

    for( x = 0; x < ERR_LEN; x++ ){
        if(x < string_length){
            t.attribute.err_lex[x] = lexeme[x];
        }
    }

    t.attribute.err_lex[string_length] = '\0';

    return t;
}


/***********************************************************************
 * Purpose:             ACCEPTING FUNCTION FOR THE ERROR TOKEN WITH RETRACT
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    strlen()
 * Parameters:          char array [ ] lexeme
 * Return value:        tempBuffer
 * Algorithm:           THE FUNCTION SETS THE ERROR TOKEN. lexeme[] CONTAINS THE ERROR
 *                      THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme ITSELF
 *                      AND IT MUST BE STORED in err_lex.  IF THE ERROR LEXEME IS LONGER
 *                      than ERR_LEN caharacters, only the first ERR_LEN character are
 *                      stored in err_lex.
 **********************************************************************/
Token aa_func13(char lexeme[]){

    Token t;            /* create token to pass to calling function */
    int x;              /* for itteration */
    int string_length;  /* for holding size of lexeme length */

    /* get string length */
    string_length = strlen(lexeme);

    t.code = ERR_T;

    for( x = 0; x < ERR_LEN; x++ ){
        if(x < string_length){
            t.attribute.err_lex[x] = lexeme[x];
        }
    }

    t.attribute.err_lex[string_length] = '\0';

    return t;
}


/***********************************************************************
 * Purpose:             CONVERSION FUNCTION CONVERTS AN ASCII STRING REPRESENTING AN OCTAL INTEGER CONSTANT TO INTEGER VALUE
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    strtol()
 * Parameters:          char array [ ] lexeme
 * Return value:        tempBuffer
 * Algorithm:           Creates a string to store excess text from strtol
 *                      Creates a long to store the int value of the octal string
 *                      If the parameter text is 0, the octal value is zero
 *                      If the strtol operation is successful, return the octal value as an int
 *                      If the operation was not successful, return error flag
 **********************************************************************/
long atool(char * lexeme){
    long value;                     /* long value used to store decimal value of the octal string returned from strtol */

    value = strtol(lexeme, NULL, 8);

    return value;
}
