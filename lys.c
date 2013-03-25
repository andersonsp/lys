#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>
#include "lys.h"

/*
 * Interpreter for Lys scripting language.
 * BUILD: gcc lys.c -o lys
 * USAGE: lys file.lys
 *
 * The grammar of Lys in EBNF is:
 *
 *  <comments> ::= "#" ... "\n" .
 *  <program> ::= {<statement>} .
 *  <statement> ::= "if" <paren_expr> <statement>
 *                  | "if" <paren_expr> <statement> "else" <statement>
 *                  | "while" <paren_expr> <statement>
 *                  | "do" <statement> "while" <paren_expr> ";"
 *                  | "{" { <statement> } "}"
 *                  | <expr> ";"
 *                  | ";" .
 *  <paren_expr> ::= "(" <expr> ")" .
 *  <expr> ::= <test> | <id> "=" <expr> .
 *  <test> ::= <sum> | <sum> "<" <sum> | <sum> ">" <sum> .
 *  <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term> .
 *  <term> ::= <id> | <int> | <paren_expr> .
 *  <id> ::= "a" | "b" | "c" | "d" | ... | "z" .
 *  <int> ::= <an_unsigned_decimal_integer> .
 */

// Taken from Lua  ---- Instruction encoding for a "simple" register based VM
#define SIZE_C    9
#define SIZE_B    9
#define SIZE_Bx   (SIZE_C + SIZE_B)
#define SIZE_A    9
#define SIZE_OP   5

#define POS_OP    0
#define POS_A   (POS_OP + SIZE_OP)
#define POS_C   (POS_A + SIZE_A)
#define POS_B   (POS_C + SIZE_C)
#define POS_Bx    POS_C

#define MAXARG_Bx   ((1<<SIZE_Bx)-1)
#define MAXARG_sBx  (MAXARG_Bx>>1)

#define CREATE_ABC(o,a,b,c) ( (((uint32_t) o) <<POS_OP) | (((uint32_t) a) <<POS_A) \
                              | (((uint32_t) b) <<POS_B) | (((uint32_t) c) <<POS_C) )
#define CREATE_ABx(o,a,bc) ( (((uint32_t) o)<<POS_OP) | (((uint32_t) a)<<POS_A) | (((uint32_t) bc)<<POS_Bx) )
#define CREATE_AsBx(o,a,bc) ( (((uint32_t) o)<<POS_OP) | (((uint32_t) a)<<POS_A) | (((uint32_t) (bc+MAXARG_sBx))<<POS_Bx) )

#define MASK1(n,p)    ((~((~(uint32_t)0)<<n))<<p)
#define GET_OPCODE(i) ( (int) (((i)>>POS_OP) & MASK1(SIZE_OP,0)))
#define GETARG_A(i)   ( ((int) ((i)>>POS_A) & MASK1(SIZE_A,0)))
#define GETARG_B(i)   ( ((int) ((i)>>POS_B) & MASK1(SIZE_B,0)))
#define GETARG_C(i)   ( ((int) ((i)>>POS_C) & MASK1(SIZE_C,0)))
#define GETARG_Bx(i)  ( ((int) ((i)>>POS_Bx) & MASK1(SIZE_Bx,0)))
#define GETARG_sBx(i) (GETARG_Bx(i)-MAXARG_sBx)

#define BITRK   (1 << (SIZE_B - 1))
#define ISK(x)    ((x) & BITRK)
#define INDEXK(r) ((int)(r) & ~BITRK)
#define MAXINDEXRK  (BITRK - 1)
#define RKASK(x)  ((x) | BITRK)

//
// Object defs
//
typedef int Object;

//
// Lexer.
//
enum { DO_SYM = 256, ELSE_SYM, IF_SYM, WHILE_SYM, INT, ID, EOI };
char *words[] = { "do", "else", "if", "while", NULL };

FILE* fin;
int ch = ' ';
int sym, last_sym, line = 1;
int int_val;
char id_name[100];

void syntax_error( const char* str, ... ) {
    fprintf(stderr, "Syntax error in line %d:\n", line );
    if( !str ) exit(1);

    va_list a;
    va_start(a, str);

    char msg[1024] = {0};
    vsnprintf(msg, 1024, str, a);
    fputs("  ", stderr);
    fputs(msg, stderr);
    exit(1);
}

void next_ch() {
    ch = fgetc( fin );
}

void next_sym() {
    again: switch (ch) {
        case ' ': case '\t': case '\r':
            next_ch();
            goto again;
        case EOF: sym = EOI; break;
        case '#':
            do next_ch(); while( ch != '\n' && ch != EOF );
        case '\n':
            line++;
            next_ch();
            goto again;
        case '{': case '}':
        case '(': case ')':
        case '+': case '-':
        case '<': case '>':
        case ';':
        case '=':
            sym = ch;
            next_ch();
            break;
        default:
            if( ch >= '0' && ch <= '9' ) {
                int_val = 0; // missing overflow check
                while( ch >= '0' && ch <= '9' ) {
                    int_val = int_val*10 + (ch - '0');
                    next_ch();
                }
                sym = INT;
            } else if( ch >= 'a' && ch <= 'z' ) {
                int i = 0; // missing overflow check
                while( (ch >= 'a' && ch <= 'z') || ch == '_' ) {
                    id_name[i++] = ch;
                    next_ch();
                }
                id_name[i] = '\0';
                sym = 0;
                while( words[sym] != NULL && strcmp(words[sym], id_name) != 0 ) sym++;
                if( words[sym] == NULL ) {
                    if( id_name[1] == '\0' ) sym = ID;
                    else syntax_error( "Invalid identifier\n" );
                } else {
                    sym += 256;
                }
            } else {
                syntax_error("Unknown token: %c\n", ch);
            }
    }
}

//
// Parser & Compiler.
//
enum {
    OP_HALT, OP_MOVE, OP_LOADK, OP_LOADNFT, OP_GETGLOBAL, OP_SETGLOBAL,
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_POW, OP_UNM, OP_NOT,
    OP_JMP, OP_JZ, OP_AND, OP_OR, OP_EQ, OP_NEQ, OP_LT, OP_LE, OP_GT, OP_GE,
    OP_CALL, OP_RET
};

#define SIZEK 512
#define SIZEC 1024*1024

Object constants[ SIZEK ];
uint32_t code[ SIZEC ] = {0};
int topk = 0, topc = 0, sp = 26;

static int geti() {
    // maybe grow the size of code
    topc++;
    return topc-1;
}

static int getk() {
    // maybe grow the size of constants area
    topk++;
    return topk-1;
}

static int paren_expr(); // forward declaration

static int term() {  // <term> ::= <id> | <int> | <paren_expr>
    int a = 0;
    last_sym = sym;
    if( sym == ID ) {
        a = id_name[0]-'a';
        next_sym();
    } else if( sym == INT ) {
        a = getk();
        constants[ a ] = int_val;
        next_sym();
        a = RKASK( a );
    } else {
        a = paren_expr( 0 ); // don't eval expression in parents ?
    }
    return a;
}

static int sum() { // <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
    int i, b, c, a = term();
    while( sym == '+' || sym == '-' ) {
        b = a;
        a = sp++;
        i = (sym == '+' ? OP_ADD : OP_SUB);
        next_sym();
        c = term();
        last_sym = '+';
        int x = geti();
        code[x] = CREATE_ABC( i, a, b, c );
    }
    return a;
}

static int test() { // <test> ::= <sum> | <sum> "<" <sum>
    int i, b, c, a = sum();
    if( sym == '<' || sym == '>' ) { // FIXME ??
        b = a;
        a = sp++;       //find a temporal register to store the result
        i = ( sym == '<' ? OP_LT : OP_GT );
        next_sym();
        c = sum();
        last_sym = '<';
        int x = geti();
        code[x] = CREATE_ABC( i, a, b, c );
    }
    return a;
}

static int expr() {  // <expr> ::= <test> | <id> "=" <expr>
    int b, a, saved_sp = sp;
    if( sym == ID ) {
        a = test();
        if( last_sym == ID && sym == '=' ) {
            next_sym();
            b = expr();
            int x = geti();
            code[x] = CREATE_ABC( OP_MOVE, a, b, 0 );
        }
    } else {
        a = test();
    }

    sp = saved_sp;
    return a;
}

static int paren_expr( int eval ) { // <paren_expr> ::= "(" <expr> ")"
    int a, b;

    if( sym == '(') next_sym();
    else syntax_error( "expected '('\n" );
    a = expr();

    if( ISK(a) && eval ) {
        int x = geti();
        b = a;
        a = sp++;
        code[x] = CREATE_ABx( OP_LOADK, a, b );
    }

    if( sym == ')' ) next_sym();
    else syntax_error( "expected ')'\n" );

    return a;
}

static int statement() {
    int x, b, c, a = 0;
    float aux;
    int saved_sp;

    if( sym == IF_SYM ) {  // "if" <paren_expr> <statement>
        next_sym();
        a = paren_expr( 1 );
        x = geti();
        c = statement();
        code[x] = CREATE_AsBx( OP_JZ, a, topc-x-1 );

        if( sym == ELSE_SYM ) { // ... "else" <statement>
            code[x] = CREATE_AsBx( OP_JZ, a, topc-x );
            x = geti();
            next_sym();
            statement();
            code[x] = CREATE_AsBx( OP_JMP, a, topc-x-1 );
        }

    } else if( sym == WHILE_SYM ) {  // "while" <paren_expr> <statement>
        next_sym();
        b = topc;
        a = paren_expr( 1 );
        c = geti();
        statement();
        x = geti();
        code[x] = CREATE_AsBx( OP_JMP, 0, b-topc );
        code[c] = CREATE_AsBx( OP_JZ, a, topc-c-1 );

    } else if( sym == DO_SYM ) {  // "do" <statement> "while" <paren_expr> ";"
        next_sym();
        b = topc;
        statement();
        if( sym == WHILE_SYM ) next_sym();
        else syntax_error( "expected 'while' token\n" );
        a = paren_expr( 1 );
        if( sym == ';' ) next_sym();
        else syntax_error( "expected ';'\n" );
        x = geti();
        code[x] = CREATE_AsBx( OP_JZ, a, 1 );
        x = geti();
        code[x] = CREATE_AsBx( OP_JMP, 0, b-topc );

    } else if( sym == ';' ) { // ";"
        next_sym();

    } else if( sym == '{' ) {  // "{" { <statement> } "}"
        saved_sp = sp;

        next_sym();
        while( sym != '}' ) statement();
        next_sym();

        sp = saved_sp;

    } else { // <expr> ";"
        expr();
        if( sym == ';' ) next_sym();
        else syntax_error( "in expression expected ';'\n" );
    }

    return a;
}

static int program() { // <program> ::= <statement>
    int a;
    next_sym();
    while( sym != EOI ) a = statement();
    if( sym != EOI ) syntax_error( "expected EOF\n" );
    return a;
}

//
// Virtual machine.
//
// Object globals[26];
Object stack[1024];

#define RKB(i) ( ISK(GETARG_B(i)) ? &constants[INDEXK(GETARG_B(i))] : &stack[GETARG_B(i)] )
#define RKC(i) ( ISK(GETARG_C(i)) ? &constants[INDEXK(GETARG_C(i))] : &stack[GETARG_C(i)] )

int ip = 0;
void run() {
    uint32_t i, op, done = 0;
    int a, b, c, e;       // opcode args
    Object *rb, *rc, *ob; // we are working only with integers for now

    do {
        i = code[ ip++ ];
        op = GET_OPCODE(i);
        a = GETARG_A(i);
        if( op >= OP_MOVE || op <= OP_AND ) {
            rb = RKB(i);
            rc = RKC(i);
        }

        switch( op ) {
            case OP_HALT:
                done = 1;
                break;
            case OP_LOADK:
                stack[a] = constants[GETARG_Bx(i)];
                break;
            case OP_MOVE:
                stack[ a ] = *rb;
                break;
            case OP_ADD:
                stack[ a ] = *rb + *rc;
                break;
            case OP_SUB:
                stack[ a ] = *rb - *rc;
                break;
            case OP_LT:
                stack[ a ] = (*rb < *rc);
                break;
            case OP_GT:
                stack[ a ] = (*rb < *rc);
                break;
            case OP_JMP:
                ip += GETARG_sBx(i);
                break;
            case OP_JZ:
                if( stack[ a ] == 0  )
                    ip += GETARG_sBx(i);
                break;
        }

    } while( !done && ip <= SIZEC );
}

void usage(){
    printf("USAGE:\n  lys file.lys\n");
}

//
// Main program.
//
int main( int argc, char**argv ) {
    if( argc < 2 ) {
        usage();
        return 0;
    }

    fin = fopen( argv[1], "r" );
    program();

    int i;
    for( i=0; i<26; i++ ) stack[i] = 0;
    run();
    for( i=0; i<26; i++ ) {
        if( stack[i] != 0 )
            printf( "%c = %d\n", 'a'+i, stack[i] );
    }

  return 0;
}
