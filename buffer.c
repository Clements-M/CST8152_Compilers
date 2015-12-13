/***********************************************************************
 * File name:       buffer.c
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
 * WARNINGS:        3 Warnings in Visual Studio
 **********************************************************************/
#include "buffer.h"


/***********************************************************************
 * Purpose:             creates a Buffer structure and sets values with given parameters
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    malloc()
 *                      free()
 *                      calloc()
 * Parameters:          init_capacity:  short   (ZERO to SHRT_MAX)
 *                      inc_factor:     char    (multiplicative: MIN_RANGE_1 to MAX_RANGE_100)
 *                                              (addative: 1 to 255)
 *                      o_mode:         char    (f, a, m)
 * Return value:        pointer to Buffer
 * Algorithm:           test for errors if true return null
 *                      create Buffer (pBD) and char array (pCharArray) in dynamic memory
 *                      sets the member values according to given parameters
 *                      return pointer to a Buffer (pBD)
 **********************************************************************/
Buffer* b_create (short init_capacity,char inc_factor,char o_mode) {

    Buffer* pBD;            /* for pointing to a new Buffer structure */
    char*   pCharArray;     /* for pointing to a char array for the Buffer */

    /* check for parameter issues */
    if(init_capacity < ZERO) {

        return NULL;

    }

    /* check if mode has legal content */
    if(o_mode != 'f' && o_mode != 'a' && o_mode != 'm') {

        return NULL;

    }

    /* if initial capacity of fixed mode is zero, there is no room in buffer */
    if (o_mode == 'f' && init_capacity <= 0) {

        return NULL;

    }

    /* create a new buffer */
    pBD = (Buffer*)calloc(ONE, sizeof(Buffer));

    if(pBD == NULL) {

        return NULL;

    }

    /* create a new character array for the buffer */
    pCharArray = (char*)malloc(init_capacity * sizeof(char));

    /* tests for error and if true, free pBD, return NULL */
    if (pCharArray == NULL) {

        free(pBD);

        return NULL;

    }

    /* set pointer cb_head to pCharArray */
    pBD->cb_head = pCharArray;

    /* if mode is 'f' or if inc_factor is zero make the other to as well */
    if (o_mode == 'f' || inc_factor == ZERO){

        pBD->mode = FIXED;
        pBD->inc_factor = ZERO;

    }

    /* this if statement may be redundant if o_mode is tested first on previous */
    /* force inc_factor to become zero to conform to fixed mode */
    else if (o_mode == 'f' && inc_factor != ZERO){

        pBD->mode = FIXED;
        pBD->inc_factor = ZERO;

    }

    /* the mode is set to 'a' and the buffer inc_factor is set to the value of inc_factor */
    else if(o_mode == 'a' && (unsigned char)inc_factor > 0){

        pBD->mode = ADDITIVE;
        pBD->inc_factor = inc_factor;

    }

    /* the mode is set to number -1 and the inc_factor value is assigned to the buffer inc_factor */
    else if(o_mode == 'm' && inc_factor >= MIN_RANGE_1 && inc_factor <= MAX_RANGE_100){

        pBD->mode = MULTIPLICATIVE;
        pBD->inc_factor = inc_factor;

    }

    /* handle errors and return NULL */
    else {

        /* free char array */
        free(pBD->cb_head);
        /* free buffer structure */
        free(pBD);

        return NULL;

    }

    pBD->capacity = init_capacity;

    return pBD;  /* pBD is of type Buffer* */

}


/***********************************************************************
 * Purpose:             adds the character symbol to the character array of buffer
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    realloc()
 *                      b_isfull()
 *                      b_mode()
 * Parameters:          pBD:    pBuffer     (Range not applicable)
 *                      symbol: char        (Signed -128 to 127, Unsigned: 0 to 255)
 * Return value:        pBD:    pBuffer     (if symbol was added to char array)
 *                      NULL                (for everything else)
 * Algorithm:           test for errors if true return null
 *                      set r_flag to ZERO
 *                      if charArray is not full add symbol to charArray, return pointer to buffer
 *                      if charArray is full, check mode and increase the size of the buffer according to mode specifications
 *                      return NULL if charArray cannot be increased
 *                      else increase charArray, add symbol, set members to new information, return pointer to buffer
 **********************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {

    char* tempCharArray;    /* points to new char array on realloc */
    short availableSpace;   /* for testing available space */
    short newIncrement;     /* for holding new increment size */
    short newCapacity;      /* for holding new capacity size */

    if(pBD == NULL) {

        return NULL;

    }

    /* r_flag to 0 */
    pBD->r_flag = RESET_R_FLAG;

    /* If the buffer is operational and is not full, add symbol to buffer */
    if(!(pBD->capacity == pBD->addc_offset)) {

        pBD->cb_head[pBD->addc_offset] = symbol;
        pBD->addc_offset++;

        return pBD;

    }

    /* else increase the size of the buffer according to set mode */
    switch(b_mode(pBD)) {

        /* FIXED (0) */
        case FIXED :
            return NULL;
            break;

        /* ADDITIVE (1) */
        case ADDITIVE :
            if((newCapacity = pBD->capacity + (unsigned char)pBD->inc_factor * sizeof(char)) < ZERO) {

                return NULL;

            }

            break;

        /* MULTIPLICATIVE (-1) */
        case MULTIPLICATIVE :
            /* test if buffer capacity is maximum size, return null */
            if(pBD->capacity == SHRT_MAX) {

                return NULL;

            }

            /* calculate the new capacity of the buffer */
            availableSpace = SHRT_MAX - pBD->capacity;
            newIncrement = (short)((float)availableSpace * pBD->inc_factor / 100.0f);
            newCapacity = pBD->capacity + newIncrement;

            /* test if new increment is zero or is equal to new capacity */
            if(newCapacity == pBD->capacity) {

                newCapacity = SHRT_MAX;

            }

        break;

    } /* end switch */

    /* realloc char array to new capacity */
    tempCharArray = (char*)realloc(pBD->cb_head, newCapacity);

    if(tempCharArray == NULL){

        return NULL;

    }

    /* if new char array has moved set r flag */
    if(pBD->cb_head != tempCharArray) {

        pBD->r_flag = SET_R_FLAG;
        pBD->cb_head = tempCharArray;

    }

    pBD->cb_head[pBD->addc_offset] = symbol;
    pBD->addc_offset++;
    pBD->capacity = newCapacity;

    return pBD;

}


/***********************************************************************
 * Purpose:             resets Buffer structure offsets, eob and r_flag to ZERO
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        EXIT_SUCCESS
 * Algorithm:           test for errors
 *                      reset offsets, eob and r_flag to ZERO, EXIT_SUCCESS (0)
 **********************************************************************/
int b_reset (Buffer * const pBD) {

    if(pBD == NULL) {

        return R_FAIL_1;

    }

    /* clear instructions */
    pBD->addc_offset = ZERO;
    pBD->getc_offset = ZERO;
    pBD->mark_offset = ZERO;
    pBD->eob = ZERO;
    pBD->r_flag = ZERO;

    return EXIT_SUCCESS;

}


/***********************************************************************
 * Purpose:             frees charArray & buffer structure from dynamic memory
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    free()
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        void
 * Algorithm:           tests for !NULL frees charArray & buffer structure
 **********************************************************************/
void b_destroy(Buffer * const pBD) {

    /* clear instructions */
    if (pBD != NULL) {

        free(pBD->cb_head);
        free(pBD);

    }

    return;

}


/***********************************************************************
 * Purpose:             checks if charArray is full and returns true or false
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        True or False (0 or 1)
 * Algorithm:           test for errors, if true return R_FAIL_1 (-1)
 *                      test to see if there is available space
 *                      return TRUE/FALSE
 **********************************************************************/
#ifndef B_FULL_MACRO /* if B_FULL_MACRO is not defined use this function instead */
int b_isfull (Buffer * const pBD) {

    if (pBD == NULL) {

        return R_FAIL_1;

    }

    /* test if buffer is full */
    if (pBD->capacity == pBD->addc_offset) {

        return TRUE;

    }

    else {

        return FALSE;
    }

}
#endif /* B_FULL_MACRO */


/***********************************************************************
 * Purpose:             returns size of char string in buffer
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        returns size of char array in buffer
 * Algorithm:           test for errors, if true, return R_FAIL_1 (-1)
 *                      return size of char array in buffer
 **********************************************************************/
short b_size (Buffer * const pBD) {

    if (pBD == NULL) {

        return R_FAIL_1;

    }

    return pBD->addc_offset;

}


/***********************************************************************
 * Purpose:             returns capacity of buffer
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        capacity of buffer
 * Algorithm:           test for errors, if true, return R_FAIL_1 (-1)
 *                      return capacity of char array in buffer
 **********************************************************************/
short b_capacity(Buffer * const pBD) {

    if (pBD == NULL){

        return R_FAIL_1;

    }

    return pBD->capacity;

}


/***********************************************************************
 * Purpose:             sets buffer member mark_offset to parameter mark
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    b_size()
 * Parameters:          pBD:    Buffer* (Range not applicable)
 *                      mark:   short   (-32,768 to 32,767)
 * Return value:        returns mark_offset
 * Algorithm:           test for errors, if true, return fail (NULL)
 *                      test if char array of buffer is empty, if true, set mark_offset to addc_offset
 *                      else test if mark is zero to content, set mark_offset to mark
 *                      returns a pointer to the currently set mark_offset location in the buffer character array
 *                      fail on errors (NULL)
 **********************************************************************/
char* b_setmark (Buffer * const pBD, short mark) {

    if (pBD == NULL) {

        return NULL;

    }

    /* test if mark is >= ZERO and <= size of char array content */
    else if (mark >= ZERO && mark <= pBD->addc_offset) {

        pBD->mark_offset = mark;

    }

    else {

        return NULL;

    }

    return pBD->cb_head + pBD->mark_offset;

}


/***********************************************************************
 * Purpose:             returns buffer mark_offset
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        returns mark_offset
 * Algorithm:           test for errors, if true, return fail (-1)
 *                      return buffer mark_offset
 **********************************************************************/
short b_mark (Buffer * const pBD) {

    if (pBD == NULL) {

        return R_FAIL_1;

    }

    return pBD->mark_offset;

}


/***********************************************************************
 * Purpose:             returns buffer mode
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        returns buffer mode
 * Algorithm:           test for errors, if true, return fail (-2)
 *                      return buffer mode
 **********************************************************************/
int b_mode (Buffer * const pBD) {

    if (pBD == NULL) {

        return R_FAIL_2;

    }

    return pBD->mode;

}


/***********************************************************************
 * Purpose:             returns the non-negative value of inc_factor
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:    Buffer* (Range not applicable)
 * Return value:        size_t or FAIL_256 (on failure)
 * Algorithm:           test for errors, if true, return 256
 *                      returns the non-negative value of inc_factor (cast as unsigned char then size_t)
 *
 * WARNINGS:            format warning when using [-Wformat=]
 *                      (warning: format ‘%u’ expects argument of type ‘unsigned int’,
 *                      but argument 2 has type ‘size_t {aka long unsigned int}’ [-Wformat=])
 **********************************************************************/
size_t b_inc_factor (Buffer * const pBD) {

    /* test for buffer error */
    if(pBD == NULL) {

        return FAIL_256;

    }

    return (size_t)(unsigned char)pBD->inc_factor;

}


/***********************************************************************
 * Purpose:             loads file character by character into the buffer
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    feof(),
 * Parameters:          fi:     FILE*   (Range not applicable)
 *                      pBD:    Buffer* (Range not applicable)
 * Return value:        numChars
 * Algorithm:           test for errors, if true, return error type
 *                      get char from file
 *                      while not end of file: (loop)
 *                          -add char to buffer char array
 *                          -add one to number of chars added to array
 *                      return numChars added to buffer char array
 **********************************************************************/
int b_load (FILE * const fi, Buffer * const pBD) {

    int numChars = ZERO;    /* holds number of characters counted */
    char newChar;           /* holds next charater recieved from file */

    /* test for buffer error */
    if(pBD == NULL) {

        return R_FAIL_1;

    }

    /* test for file error */
    if(fi == NULL) {

        return LOAD_FAIL;

    }

    /* get char from file */
    newChar = fgetc(fi);

    /* loop through file until end of file is reached */
    while (!feof(fi)) {

        /* add char to buffer char array and increment numChars added */
        if(!b_addc(pBD, newChar)) {
            return LOAD_FAIL;
        }
        numChars++;
        newChar = fgetc(fi);

    }

    return numChars;

}


/***********************************************************************
 * Purpose:             tests if buffer char array is empty
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:  Buffer*   (Range not applicable)
 * Return value:        TRUE (1) or FALSE (0)
 * Algorithm:           test for errors, if true, retrun R_FAIL_1 (-1)
 *                      test if buffer is empty:
 *                      if true, return TRUE (1)
 *                      else return FALSE (0)
 **********************************************************************/
int b_isempty (Buffer * const pBD) {

    if (pBD == NULL) {

        return R_FAIL_1;

    }

    /* test to see if buffer char array is empty */
    if(pBD->addc_offset == ZERO) {

        return TRUE;

    }

    return FALSE;

}


/***********************************************************************
 * Purpose:             returns end of buffer flag
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        eob: int
 * Algorithm:           test for errors, if true, return R_FAIL_1 (-1)
 *                      else return eob (end of buffer) flag
 **********************************************************************/
int b_eob (Buffer * const pBD) {

    if(pBD == NULL) {

        return R_FAIL_1;
    }

    return pBD->eob;

}


/***********************************************************************
 * Purpose:             returns the character at getc_offset
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        tempChar: char
 * Algorithm:           test for errors, if true, return R_FAIL_2 (-2)
 *                      if buffer getc_offset equals buffer addc_offset set end of buffer to ONE (1), return R_FAIL_1 (-1)
 *                      else set end of buffer to ZERO (0), get char at getc_offset, increment getc_offset
 *                      return char
 **********************************************************************/
char b_getc (Buffer * const pBD) {

    char tempChar;

    if (pBD == NULL){

        return R_FAIL_2;

    }

    /* test if buffer getc_offset equals buffer addc_offset set end of buffer to ONE (1), return R_FAIL_1 (-1) */
    if(pBD->getc_offset == pBD->addc_offset) {

        pBD->eob = ONE;

        return R_FAIL_1;

    }

    /* else set end of buffer to ZERO (0), get char at getc_offset, increment getc_offset, return char */
    pBD->eob = ZERO;
    tempChar = pBD->cb_head[pBD->getc_offset];
    pBD->getc_offset++;

    return tempChar;

}


/***********************************************************************
 * Purpose:             prints the contents of the character buffer and returns the number of characters read
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    b_isempty()
 *                      b_getc()
 *                      b_eob()
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        charCounter
 * Algorithm:           test for errors, if true, return R_FAIL_1 (-1)
 *                      if buffer is empty, print buffer empty string
 *                      set string offset to ZERO (0), get next char from buffer char array
 *                      while not the end of buffer: (loop)
 *                          -print char
 *                          -add one to number of chars added to array
 *                          -get next char from buffer char array
 *                      set getc_offset to ZERO (0)
 *                      return charArray (number of chars printed)
 **********************************************************************/
int b_print (Buffer * const pBD) {

    int charCounter = ZERO;     /* holds number of characters counted */
    char newChar;               /* holds next charater recieved from file */

    if (pBD == NULL){

        return R_FAIL_1;

    }

    /* test if buffer is empty, print buffer empty string */
    if (b_isempty(pBD)) {

        printf("The buffer is empty.");

    }

    /* set string offset to ZERO (0), get next char from buffer char array */
    else {

        pBD->getc_offset = ZERO;

        newChar = b_getc(pBD);

        /* while not the end of buffer: print char, add one to charCounter, get next char */
        while(!b_eob(pBD)) {

            printf("%c", newChar);
            charCounter++;
            newChar = b_getc(pBD);

        }

        pBD->getc_offset = ZERO;

    }

    printf("\n");

    return charCounter;

}


/***********************************************************************
 * Purpose:             resizes character buffer to currently used capacity + ONE (1)
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        pBD (pointer to buffer)
 * Algorithm:           test for errors, if true, return NULL
 *                      set newCapacity to addc_offset + ONE (1)
 *                      set r_flag to ZERO (0)
 *                      realloc char array to size of new charCapacity (test for error)
 *                      set buffer capacity to newCapacity, set r_flag to SET_R_FLAG (1)
 **********************************************************************/
Buffer *b_pack(Buffer * const pBD) {

    short newCapacity;      /* holds new capacity that charater array will be resized to */
    char* tempBuffer;       /* for pointing to the new realloc'd character array */

    if (pBD == NULL) {

        return NULL;

    }

    /* reset r_flag */
    pBD->r_flag = ZERO;

    /* test for type overflow */
    if (pBD->addc_offset == SHRT_MAX) {

        return NULL;

    }

    /* newCapacity is addc_offest plus one converted into bytes */
    newCapacity  = ((pBD->addc_offset + ONE) * sizeof(char));

    /* realloc to newCapacity then update all necessary members of the BufferDescriptor structure */
    if((tempBuffer = (char*)realloc(pBD->cb_head, newCapacity)) == NULL) {

        return NULL;

    }

    if(tempBuffer != pBD->cb_head) {

        pBD->cb_head = tempBuffer;
        pBD->r_flag = SET_R_FLAG;
        b_addc(pBD, '\0');

    }

    pBD->capacity = newCapacity;

    return pBD;

}


/***********************************************************************
 * Purpose:             returns buffer r_flag
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        r_flag
 * Algorithm:           test for error, if true, return R_FAIL_1 (-1)
 *                      return buffer r_flag
 **********************************************************************/
char b_rflag (Buffer * const pBD) {

    if (pBD == NULL) {

        return R_FAIL_1;

    }

    return pBD->r_flag;

}


/***********************************************************************
 * Purpose:             retracts offset to offset - 1
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        returns getc_offset
 * Algorithm:           test for errors, if true, return R_FAIL_1 (-1)
 *                      if getc_offset is not ZERO (0), decrement getc_offset
 *                      return getc_offset
 *                      else return R_FAIL_1 (-1)
 **********************************************************************/
short b_retract (Buffer * const pBD) {

    if (pBD == NULL) {

        return R_FAIL_1;

    }

    /* if getc_offset is not ZERO (0), decrement getc_offset, return getc_offset */
    if (pBD->getc_offset > ZERO) {

        pBD->getc_offset--;

        return pBD->getc_offset;

    }

    return R_FAIL_1;

}


/***********************************************************************
 * Purpose:             retracts offset to mark_offset
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD: Buffer*    (Range not applicable)
 * Return value:        getc_offset (returns new offset)
 * Algorithm:           test for error, if true, return R_FAIL_1 (-1)
 *                      set getc_offset to mark_offset
 *                      return getc_offset
 **********************************************************************/
short b_retract_to_mark (Buffer * const pBD) {

    if (pBD == NULL){

        return R_FAIL_1;

    }

    pBD->getc_offset = pBD->mark_offset;

    return pBD->getc_offset;

}


/***********************************************************************
 * Purpose:             returns getc_offset
 * Author:              Cory Hilliard
 * History/Versions:    1.0
 * Called functions:    none
 * Parameters:          pBD:  Buffer*   (Range not applicable)
 * Return value:        getc_offset
 * Algorithm:           test for error, if true, return R_FAIL_1 (-1)
 *                      return getc_offset
 **********************************************************************/
short b_getc_offset (Buffer * const pBD) {

    if (pBD == NULL){

        return R_FAIL_1;

    }

    return pBD->getc_offset;

}

