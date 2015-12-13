/***********************************************************************
 * File name:       stable.c
 * Compiler:        gcc
 * IDE:             Code::Blocks version 13.12 on (mixed environments)
 * Authors:         Cory Hilliard    040 630 141 (Linux - Fedora 23)
 *                  Matthew Clements 040 766 220 (Windows - v8)
 * Course:          CST8152 - Compilers, Lab Section: 011
 * Assignment:      03 Symbol Table
 * Due Date:        Friday November 20 2015
 * Professor:       Svillen Ranev
 * Purpose:         Implementing and Incorporating a Symbol Table
 * Function list:   st_create()
 **********************************************************************/
#include <string.h>
#include "stable.h"

extern STD sym_table;  /* defined in platy_tt.c */
static void st_setsize(void);
static void st_incoffset(void);
static unsigned short set_all_bits_default(unsigned short);
static unsigned short set_00(unsigned short,unsigned short);
static unsigned short set_0201(unsigned short,unsigned short);

 /***********************************************************************
 * Purpose:             Creates the symbol table with correct info
 * Author:              Matthew Clements 040 766 220
 * Author:              Cory Hilliard    040 630 141
 * History/Versions:    1.0
 * Called functions:    malloc(), sizeof(), b_create(), free()
 * Parameters:          int st_size
 * Return value:        STD sym_table_desc
 * Algorithm:           attempts to allocate memory, then creates STD.buffer,
 *                      sets STD.st_offset to 0, then STD.st_size to parameter size
 **********************************************************************/
STD st_create( int st_size ) {

	STD sym_table;      /* create STD */
	STVR* tempPointer;  /* create a temp pointer for STVR array */

    /* check parameter validity */
    if ( st_size <= 0 ) {

        sym_table.st_size = 0;
		return sym_table;
    }

	/* allocate an array of size STVR * st_size */
	tempPointer = (STVR*)malloc(sizeof(STVR) * st_size);

	/* test for null */
	if( tempPointer == NULL ) {

		sym_table.st_size = 0;
		return sym_table;

	}

    /* set the array of records to the valid pointer */
    sym_table.pstvr = tempPointer;

    /* create a character array buffer to hold lexemes */
	sym_table.plsBD = b_create(INIT_CAPACITY, INC_FACTOR, 'a');

    /* test for null */
    if (sym_table.plsBD == NULL) {
        free(sym_table.pstvr);
        sym_table.st_size = 0;
        return sym_table;
    }

	sym_table.st_offset = 0;
	sym_table.st_size = st_size;

	return sym_table;

}

 /***********************************************************************
 * Purpose:             installs the new lexeme at the current st_offset
 * Author:              Matthew Clements 040 766 220
 * Author:              Cory Hilliard    040 630 141
 * History/Versions:    1.0
 * Called functions:    st_lookup(), strlen(), set_all_bits_default(),
 *                      b_addc(), b_setmark, set_0201, set_00, st_incoffset
 * Parameters:          STD sym_table, char *lexeme, char type, int line
 * Return value:        int
 * Algorithm:           check if lexeme exists in buffer, if no add
 **********************************************************************/
int st_install(STD sym_table, char *lexeme, char type, int line){

    int i;                  /* for itteration */
    int lex_offset;         /* for lexeme offset */
    int lex_length;         /* for lexeme length */
    int temp_r_flag = 0;    /* for buffer reallocation */
    unsigned short temp_field = 0;  /* for holding field before putting into table */
	char* temp_pointer;

    /* check symbol table */
    if ( !sym_table.st_size ) {

        return R_FAIL_1;
    }

    /* if lexeme exists get offset */
    lex_offset = st_lookup(sym_table, lexeme);

    /* get lexeme length */
    lex_length = strlen(lexeme);

    /* set the field to default values */
    temp_field = set_all_bits_default(temp_field);

    /* if database is full, return fail -1 */
    if ( sym_table.st_size == sym_table.st_offset ) {

        return R_FAIL_1;

    }

	/* if symbol table's buffer is full, return fail -1 */
	if ( b_isfull(sym_table.plsBD) ) {

        return R_FAIL_1;

    }

    /* if lexeme is in symbol table, return record index */
    if ( lex_offset != -1 ) {

        return lex_offset;

    }

    /* test parameter for valid input */
    if ( type != 'I' && type != 'F' && type != 'S' ) {

        return R_FAIL_1;

    }
    /* store pointer before adding any characters to the buffer */
    temp_pointer = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));

    /* copy lexeme to CA buffer */
    for ( i = 0; i <= lex_length; i++ ) {

        if(b_addc(sym_table.plsBD, lexeme[i]) == NULL){
            return R_FAIL_1;
        }

        if (sym_table.plsBD->r_flag) {

            temp_r_flag = SET_R_FLAG;
        }

    }

    /* check for buffer realloc and reset plex pointers */
    if (temp_r_flag == SET_R_FLAG) {

        lex_offset = 0;

        /* fix all plex pointers */
        for ( i = 0; i < sym_table.st_offset; i++ ) {

            lex_length = strlen(sym_table.plsBD->cb_head + lex_offset);

            //TODO: Change to buffer_setmark()
            sym_table.pstvr[i].plex = sym_table.plsBD->cb_head + lex_offset;

            lex_offset += lex_length + 1;

        }

    }

    else{

        /* set current record plex to stored address */
        sym_table.pstvr[sym_table.st_offset].plex = temp_pointer;
    }

    /* sets o_line to the corresponding values */
    sym_table.pstvr[sym_table.st_offset].o_line = line;

    /* sets the data type indicator to a value corresponding to the type of the variable specified by the formal parameter type */
    /* value of the type parameter can only be I, F or S */
    if ( type == 'I') {

        /* set status to int */
        temp_field = set_0201(temp_field, SET_21_10);

        /* set the status field in array to the temp field */
        sym_table.pstvr[sym_table.st_offset].status_field = temp_field;

        /* The function sets the i_value to zero for integer */
        sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
    }

    else if ( type == 'F' ) {

        /* set status to float */
        temp_field = set_0201(temp_field, SET_21_01);

        /* set the status field in array to the temp field */
        sym_table.pstvr[sym_table.st_offset].status_field = temp_field;

        /* The function sets the i_value to zero for float */
        sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0.0f;
    }

    else {

        /* set status to string */
        temp_field = set_0201(temp_field, SET_21_11);

        /* If the variable is of type string the function sets the update flag to 1 */
        temp_field = set_00(temp_field, SET_00_1);

        /* set the status field in array to the temp field */
        sym_table.pstvr[sym_table.st_offset].status_field = temp_field;

        /* The function sets the i_value to -1 for string */
        sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
    }

    /* increment the st_offset of the “global” sym_table by 1 */
    st_incoffset();

    return sym_table.st_offset;

}

 /***********************************************************************
 * Purpose:             Search symbol table for a lexeme
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    strcmp()
 * Parameters:          STD sym_table, char *lexeme
 * Return value:        void
 * Algorithm:           loops through symbol table, comparing the
 *                      input parameter to each lexeme, if found it
 *                      returns the index of the lexeme
 *                      otherwise it returns -1
 **********************************************************************/
int st_lookup(STD sym_table, char *lexeme)
{
    int i;                              /* index counter */

    /* check symbol table */
    if ( !sym_table.st_size ) {

        return R_FAIL_1;
    }

    for( i = sym_table.st_offset - 1;                           /* set index to table size - 1 */
        i > -1 && strcmp(lexeme, sym_table.pstvr[i].plex) != 0; /* while x is in range, and the strings do not match loop */
        i-- )                                                   /* decrement the index counter by 1 every unsuccessful loop */
        {;}                                                     /* do no work no matter what */

    /*
        note about logic, if the string was not found the index will still
        be decremented therefore it will be -1 which is the fail return value
    */

    /* return index value, or -1 on fail */
    return i;

}

 /***********************************************************************
 * Purpose:             Print contents of symbol table to the console
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    printf()
 * Parameters:          STD sym_table
 * Return value:        R_FAIL_1 (-1) on fail
 * Algorithm:           Print contents of symbol table to the console
 **********************************************************************/
int st_print(STD sym_table) {

    int size = sym_table.st_offset;   /* size of the symbol table */
    int i;                            /* incrementing value */

    /* check symbol table */
    if ( !sym_table.st_size ) {

        return R_FAIL_1;
    }

    /* Output static unchanging labels for message */
    printf("\nSymbol Table\n____________\n\nLine Number Variable Identifier\n");

    /*
        loop through symbol table database
        outputting the line number
        then outputting the variable ID
        if the line number is a single digit, outputs an extra
        space at the beginning to line up
    */
    for( i = 0; i < size; i++ ){

        printf("%2d          %s\n",
                sym_table.pstvr[i].o_line,
                sym_table.pstvr[i].plex);
    }

    return (i+1);
}

 /***********************************************************************
 * Purpose:             sets the st_size to 0
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    void
 * Parameters:          void
 * Return value:        void
 * Algorithm:           Using the GLOBAL sym_table, sets st_size to 0
 **********************************************************************/
static void st_setsize(void) {

    sym_table.st_size = 0;

}

 /***********************************************************************
 * Purpose:             increments the st_offset by +1
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    void
 * Parameters:          void
 * Return value:        void
 * Algorithm:           Using the GLOBAL sym_table, adds 1 to it's offset
 **********************************************************************/
static void st_incoffset(void) {

    ++sym_table.st_offset;

}


/***********************************************************************
 * Purpose:             Stores the symbol table into a file called "$stable.ste"
 * Author:              Matthew Clements 040 766 220
 * Author:              Cory Hilliard    040 630 141
 * History/Versions:    1.0
 * Called functions:    strlen(), fopen(), fprintf(), st_get_type()
 * Parameters:          STD sym_table
 * Return value:        int -1, or (index + 1)
 * Algorithm:           attempts to open file, saves size of sym_table
 *                      into file, then loops through sym_table saving
 *                      each part of the symbol to the file
 *                      TODO: Update algorithm statement to human language
 **********************************************************************/
int st_store(STD sym_table) {

    int size = sym_table.st_size;   /* size of the symbol table */
    char *fname = "$stable.ste";    /* file name to print to */
    FILE *stf;                      /* file handler for writing to */
    int i;                          /* incrementing value */

    /* check symbol table */
    if ( !sym_table.st_size ) {

        return R_FAIL_1;
    }

    /*
        attempts to open the file in write mode:
        if file exists, overwrites
        if open not successful, returns -1

		Warning: uses fopen, which is not fully safe
    */
    if ( ( stf = fopen(fname, "w" ) ) == NULL ) {

        return -1;

    }

    /* outputs to file the size of symbol table */
    fprintf(stf, "%d", size);

    /* loops through symbol table, outputs to file each element in order:
     *  -status (hex)
     *  -lexeme length
     *  -lexeme
     *  -line number
     *  -initial value
     */
    for ( i = 0; i < size; i++ ) {

        fprintf(stf, " %4X %lu %s %d",
                sym_table.pstvr[i].status_field,
                (unsigned long)strlen(sym_table.pstvr[i].plex),
                sym_table.pstvr[i].plex,
                sym_table.pstvr[i].o_line);

        if ( ( st_get_type(sym_table, i) == 'I') ) {

            fprintf(stf, " %d", sym_table.pstvr[i].i_value.int_val);
        }

        if ( ( st_get_type(sym_table, i) == 'F') ) {

            fprintf(stf, " %.2f", sym_table.pstvr[i].i_value.fpl_val);
        }

        if ( ( st_get_type(sym_table, i) == 'S') ) {

            fprintf(stf, " %d", sym_table.pstvr[i].i_value.str_offset);

        }

    }


    fclose(stf);
    /* on success output success string, then returns index counter (plus 1) */
    printf("\nSymbol Table stored.\n");

    return (i+1);

}

/***********************************************************************
 * Purpose:             Sorts the parameter sym_table according to s_order
 * Author:              Matthew Clements 040 766 220
 * Author:              Cory Hilliard    040 630 141
 * History/Versions:    1.0
 * Called functions:    None
 * Parameters:          STD sym_table, char s_order
 * Return value:        int
 * Algorithm:           returns 0! :D
 * WARNINGS:            unused parameter ‘sym_table’ [-Wunused-parameter]
 *                      unused parameter ‘s_order’ [-Wunused-parameter]
 *                      Parameters are never used, therefore causing errors
 **********************************************************************/
int st_sort(STD sym_table, char s_order) {

    /* TODO: If attempting bonus, sort the order otherwise leave it alone */
    /* by the way, we want full credit to both of us for this! */
    return 0;
}


/***********************************************************************
 * Purpose:             updates the data type indicator in the variable entry (STVR)
 * Author:              Cory Hilliard    040 630 141
 * History/Versions:    1.0
 * Called functions:    set_0201
 * Parameters:          STD sym_table,int vid_offset,char v_type
 * Return value:        int
 * Algorithm:           returns vid_offset
 **********************************************************************/
int st_update_type(STD sym_table,int vid_offset,char v_type) {

    unsigned short temp_field = 0;  /* for holding field before putting into table */

    /* check symbol table */
    if ( !sym_table.st_size ) {

        return R_FAIL_1;
    }

    if ( v_type != 'I' && v_type != 'F') {

        return R_FAIL_1;

    }

    if ( vid_offset >= sym_table.st_offset ) {


        return R_FAIL_1;

    }

    if ( vid_offset < 0 ) {

        return R_FAIL_1;
    }

    /* if update flag is equal to 1, the type has been already updated and the function returns -1 */
    if ( ( sym_table.pstvr[vid_offset].status_field & CHK_LSB ) ) {

        return R_FAIL_1;

    }

    /* set temp_field to the current field value */
    temp_field = sym_table.pstvr[vid_offset].status_field;

    /* if the variable is of type string the function sets the update flag to 1 */
    temp_field = set_00(temp_field, SET_00_1);

    if ( v_type == 'I') {
        /* set status to int */
        temp_field = set_0201(temp_field, SET_21_10);

        /* set the status field in array to the temp field */
        sym_table.pstvr[vid_offset].status_field = temp_field;

        /* The function sets the i_value to zero for integer */
        sym_table.pstvr[vid_offset].i_value.int_val = 0;
    }

    if ( v_type == 'F' ) {
        /* set status to float */
        temp_field = set_0201(temp_field, SET_21_01);

        /* set the status field in array to the temp field */
        sym_table.pstvr[vid_offset].status_field = temp_field;

        /* The function sets the i_value to zero for float */
        sym_table.pstvr[vid_offset].i_value.fpl_val = 0.0f;
    }

    return vid_offset;

}


/***********************************************************************
 * Purpose:             updates the i_value of the variable specified by vid_offset
 * Author:              Cory Hilliard    040 630 141
 * History/Versions:    1.0
 * Called functions:    None
 * Parameters:          STD sym_table, int vid_offset, InitialValue i_value
 * Return value:        int
 * Algorithm:           returns vid_offset
 **********************************************************************/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value) {

    /* check symbol table */
    if ( !sym_table.st_size ) {

        return R_FAIL_1;
    }

    if ( vid_offset >= sym_table.st_offset ) {

        return R_FAIL_1;
    }

    if ( vid_offset < 0 ) {

        return R_FAIL_1;
    }

    sym_table.pstvr[vid_offset].i_value = i_value;

    return vid_offset;
}


/***********************************************************************
 * Purpose:             returns the type of the variable specified by vid_offset
 * Author:              Cory Hilliard    040 630 141
 * History/Versions:    1.0
 * Called functions:    None
 * Parameters:          STD sym_table, int vid_offset
 * Return value:        char
 * Algorithm:           returns I, F, S or -1
 **********************************************************************/
char st_get_type (STD sym_table, int vid_offset) {

	 unsigned short temp_field = 0;  /* for holding field while determining type */

    /* check symbol table */
    if ( !sym_table.st_size ) {

        return R_FAIL_1;
    }

    temp_field = sym_table.pstvr[vid_offset].status_field;

    temp_field = temp_field | CHK_21;

    if ( temp_field == CHK_INT) {

        return 'I';

    }

    if ( temp_field == CHK_FLT) {

        return 'F';

    }

    if ( temp_field == CHK_STR) {

        return 'S';

    }

    return R_FAIL_1;

}


/***********************************************************************
 * Purpose:             frees the memory occupied by the symbol table dynamic
 *                      areas and sets st_size to 0
 * Author:              Cory Hilliard 040 630 141
 * History/Versions:    1.0
 * Called functions:    b_destroy, free, st_setsize
 * Parameters:          STD sym_table
 * Return value:        void
 * Algorithm:           frees dynamic memory and sets size to zero
 **********************************************************************/
void st_destroy(STD sym_table) {

    /* release dynmically allocated character array buffer */
    b_destroy(sym_table.plsBD);

    /* release dynamicly allocated storage of stvr */
    free(sym_table.pstvr);

    /* set st_size to zero */
    st_setsize();

}


/***********************************************************************
 * Purpose:             Sets all bits of status_field to default values
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    set_00, set_0201
 * Parameters:          unsigned short status_field
 * Return value:        temp_field
 * Algorithm:           set bits two and one to zero, then bit zero to zero
 **********************************************************************/
static unsigned short set_all_bits_default(unsigned short status_field) {

    unsigned short temp_field = set_00(set_0201(status_field, SET_21_00), SET_00_0);
    return temp_field;
}


/***********************************************************************
 * Purpose:             Set bits two and one to bitmask
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          unsigned short status_field
 * Return value:        temp_field
 * Algorithm:           set bits two and one to bitmask
 **********************************************************************/
static unsigned short set_0201(unsigned short status_field, unsigned short bitmask) {

    unsigned short temp_field = status_field;
    temp_field = temp_field & RESET_0201;
    temp_field = temp_field | bitmask;
    return temp_field;
}


/***********************************************************************
 * Purpose:             Sets bit zero to bitmask
 * Author:              Matthew Clements 040 766 220
 * History/Versions:    1.0
 * Called functions:    None
 * Parameters:          unsigned short status_field
 * Return value:        temp_field
 * Algorithm:           set bit zero to bitmask
 **********************************************************************/
static unsigned short set_00(unsigned short status_field, unsigned short bitmask) {

    unsigned short temp_field = status_field;
    temp_field = temp_field & RESET_00;
    temp_field = temp_field | bitmask;
    return temp_field;
}
