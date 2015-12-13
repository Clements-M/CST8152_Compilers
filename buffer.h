/***********************************************************************
 * File name:       buffer.h
 * Edit Compiler:   gcc version 5.1.1 20150618 IDE Code::Blocks version 13.12 on Fedora 22
 * Test Compiler:   gcc version 4.7.1 (tdm-1)  IDE Code::Blocks version 13.12 on Windows 10
 * Author:          Cory Hilliard   040 630 141
 * Course:          CST 8152 – Compilers, Lab Section: 011
 * Assignment:      01
 * Due Date:        September 29, 2015
 * Professor:       Sv. Ranev
 * Purpose:         Program to create and manipulate a buffer
 * Function list:   b_create()              creates a Buffer struct and sets values to parameters
 *                  b_addc()                adds symbol to the character array of Buffer
 *                  b_reset()               resets Buffer structure offsets, eob and r_flag to ZERO
 *                  b_destroy()             frees charArray & buffer structure from dynamic memory
 *                  b_isfull()              tests if charArray is full and returns true or false
 *                  b_size()                returns size of char string in buffer
 *                  b_capacity()            returns capacity of buffer
 *                  b_setmark()             sets buffer member mark_offset to parameter mark
 *                  b_mark()                returns buffer mark_offset
 *                  b_mode()                returns buffer mode
 *                  b_inc_factor()          returns the non-negative value of inc_factor
 *                  b_load()                loads file character by character into the buffer
 *                  b_isempty()             tests if buffer char array is empty
 *                  b_eob()                 returns end of buffer flag
 *                  b_getc()                returns the character at getc_offset
 *                  b_print()               prints contents of the buffer and returns chars read
 *                  b_pack()                resizes character buffer to used capacity + ONE (1)
 *                  b_rflag()               returns buffer r_flag
 *                  b_retract()             retracts offset to offset - 1
 *                  b_retract_to_mark()     retracts offset to mark_offset
 *                  b_getc_offset()         returns getc_offset
 **********************************************************************/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */


/***********************************************************************
 * standard header files
 **********************************************************************/
#include <stdio.h>      /* standard input/output */
#include <malloc.h>     /* for dynamic memory allocation*/
#include <limits.h>     /* implementation-defined data type ranges and limits */
#include <stdlib.h>     /* for exit status defines */


/***********************************************************************
 * constant definitions
 **********************************************************************/
#define R_FAIL_1        -1      /* fail return value */
#define R_FAIL_2        -2      /* fail return value */
#define LOAD_FAIL       -2      /* load fail error */
#define SET_R_FLAG       1      /* realloc flag set value 1 */
#define RESET_R_FLAG     0      /* realloc flag set value 0 */
#define FIXED            0      /* indicates that the buffer operates in "fixed-size"  mode */
#define ADDITIVE         1      /* indicates "additive selfincrementing" mode */
#define MULTIPLICATIVE  -1      /* indicates "multiplicative self-incrementing" mode */
#define MIN_RANGE_1      1      /* for testing if min range of 1 */
#define MAX_RANGE_100    100    /* for testing if max range of 100 */
#define ZERO             0      /* for setting 0 */
#define ONE              1      /* for setting 1 */
#define FALSE            0      /* allowing for false == 0 */
#define TRUE             1      /* allowing for true == 1 */
#define FAIL_256         256    /* for failures in inc_factor() */
#define BUFF_FAIL       -1      /* macro failure when buffer points to NULL */


/***********************************************************************
 * Name:                B_FULL (macro)
 * Purpose:             checks if charArray is full and returns TRUE(1) or FALSE(0)
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        True or False (0 or 1)
 * Algorithm:           test for NULL pointer, if true return BUFF_FAIL (-1)
 *                      test to see if buffer is full
 *                      return TRUE/FALSE
 **********************************************************************/
/* #define B_FULL_MACRO *//* uncomment to make B_FULL() macro active */
#ifdef B_FULL_MACRO
#define B_FULL(buff_ptr) (((buff_ptr) == (NULL)) ? (BUFF_FAIL) : (buff_ptr->capacity == buff_ptr->addc_offset) ? (TRUE) : (FALSE))
#endif /* B_FULL_MACRO */


/***********************************************************************
 * user data type declarations
 **********************************************************************/
typedef struct BufferDescriptor {
    char  *cb_head;     /* pointer to the beginning of character array (character buffer) */
    short  capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short  addc_offset; /* the offset (in chars) to the add-character location */
    short  getc_offset; /* the offset (in chars) to the get-character location */
    short  mark_offset; /* the offset (in chars) to the mark location */
    char   inc_factor;  /* character array increment factor */
    char   r_flag;      /* character array reallocation flag */
    char   mode;        /* operational mode indicator*/
    int    eob;         /* end-of-buffer reached flag */
} Buffer, *pBuffer;     /* typedef Buffer *pBuffer; */


/***********************************************************************
 * function declarations
 **********************************************************************/
Buffer *b_create (short init_capacity,char inc_factor,char o_mode); /* creates a Buffer structure and sets values with given parameters */
pBuffer b_addc(pBuffer const pBD, char symbol);                     /* adds the character symbol to the character array of buffer */
int     b_reset (Buffer * const pBD);                               /* resets Buffer structure offsets, eob and r_flag to ZERO */
void    b_destroy(Buffer * const pBD);                              /* frees charArray & buffer structure from dynamic memory */
int     b_isfull (Buffer * const pBD);                              /* tests if charArray is full and returns true or false */
short   b_size (Buffer * const pBD);                                /* returns size of char string in buffer */
short   b_capacity(Buffer * const pBD);                             /* returns capacity of buffer */
char*   b_setmark (Buffer * const pBD, short mark);                 /* sets buffer member mark_offset to parameter mark */
short   b_mark (Buffer * const pBD);                                /* returns buffer mark_offset */
int     b_mode (Buffer * const pBD);                                /* returns buffer mode */
size_t  b_inc_factor (Buffer * const pBD);                          /* returns the non-negative value of inc_factor */
int     b_load (FILE * const fi, Buffer * const pBD);               /* loads file character by character into the buffer */
int     b_isempty (Buffer * const pBD);                             /* tests if buffer char array is empty */
int     b_eob (Buffer * const pBD);                                 /* returns end of buffer flag */
char    b_getc (Buffer * const pBD);                                /* returns the character at getc_offset */
int     b_print (Buffer * const pBD);                               /* prints the contents of the character buffer and returns the number of characters read */
Buffer *b_pack(Buffer * const pBD);                                 /* resizes character buffer to currently used capacity + ONE (1) */
char    b_rflag (Buffer * const pBD);                               /* returns buffer r_flag */
short   b_retract (Buffer * const pBD);                             /* retracts offset to offset - 1 */
short   b_retract_to_mark (Buffer * const pBD);                     /* reracts offset to mark_offset */
short   b_getc_offset (Buffer * const pBD);                         /* returns getc_offset */

#endif
