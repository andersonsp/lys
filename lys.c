#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lys.h"

/*
 * Interpreter for Lys scripting language.  The grammar of Lys in EBNF is:
 *
 *  <comments> ::= "#" ... "\n" .
 *  <program> ::= {<statement>}
 *  <statement> ::= "if" <paren_expr> <statement>
 *                  | "if" <paren_expr> <statement> "else" <statement>
 *                  | "while" <paren_expr> <statement>
 *                  | "do" <statement> "while" <paren_expr> ";"
 *                  | "{" { <statement> } "}"
 *                  | <expr> ";"
 *                  | ";" .
 *  <paren_expr> ::= "(" <expr> ")"
 *  <expr> ::= <test> | <id> "=" <expr>
 *  <test> ::= <sum> | <sum> "<" <sum>
 *  <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
 *  <term> ::= <id> | <int> | <paren_expr>
 *  <id> ::= "a" | "b" | "c" | "d" | ... | "z"
 *  <int> ::= <an_unsigned_decimal_integer>
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
// Lexer.
//
enum {
  DO_SYM = 256,
  ELSE_SYM,
  IF_SYM,
  WHILE_SYM,
  INT,
  ID,
  EOI
};

char *words[] = { "do", "else", "if", "while", NULL };

FILE* fin;
int ch = ' ';
int sym;
int int_val;
char id_name[100];

void syntax_error() {
    fprintf(stderr, "syntax error\n");
    exit(1);
}

void next_ch() {
    ch = fgetc( fin );
}

void next_sym() {
    again: switch (ch) {
        case EOF: sym = EOI; break;
        case '#':
            do next_ch(); while( ch != '\n' && ch != EOF );
        case ' ': case '\n': case '\t': case '\r':
            next_ch();
            goto again;
        case '{': case '}':
        case '(': case ')':
        case '+': case '-':
        case '<':
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
                    else syntax_error();
                } else {
                    sym += 256;
                }
            } else {
                syntax_error();
            }
    }
}

/*---------------------------------------------------------------------------*/

/* Parser. */

enum {
    VAR,
    CST,
    ADD,
    SUB,
    LT,
    SET,
    IF1,
    IF2,
    WHILE,
    DO,
    EMPTY,
    SEQ,
    EXPR,
    PROG
};

struct node {
    int kind;
    struct node *o1, *o2, *o3;
    int val;
};
typedef struct node node;

node *new_node(int k) {
    node *x = (node*)malloc(sizeof(node));
    x->kind = k;
    return x;
}

node *paren_expr(); // forward declaration

node *term() {  // <term> ::= <id> | <int> | <paren_expr>
    node *x;
    if( sym == ID ) {
        x = new_node(VAR);
        x->val = id_name[0]-'a';
        next_sym();
    } else if( sym == INT ) {
        x = new_node(CST);
        x->val = int_val;
        next_sym();
    } else {
        x = paren_expr();
    }
    return x;
}

node *sum() { // <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
    node *t, *x = term();
    while( sym == '+' || sym == '-' ) {
        t = x;
        x = new_node( sym == '+'?ADD:SUB );
        next_sym();
        x->o1 = t;
        x->o2 = term();
    }
    return x;
}

node *test() { // <test> ::= <sum> | <sum> "<" <sum>
    node *t, *x = sum();
    if( sym == '<' ) {
        t = x;
        x = new_node(LT);
        next_sym();
        x->o1 = t;
        x->o2 = sum();
    }
    return x;
}

node *expr() {  // <expr> ::= <test> | <id> "=" <expr>
    node *t, *x;
    if( sym != ID ) return test();
    x = test();
    if( x->kind == VAR && sym == '=' ) {
        t = x;
        x = new_node(SET);
        next_sym();
        x->o1 = t; x->o2 = expr();
    }
    return x;
}

node *paren_expr() { // <paren_expr> ::= "(" <expr> ")"
    node *x;
    if( sym == '(' ) next_sym();
    else syntax_error();
    x = expr();
    if( sym == ')' ) next_sym();
    else syntax_error();
    return x;
}

node *statement() {
    node *t, *x;
    if( sym == IF_SYM ) {  // "if" <paren_expr> <statement>
        x = new_node(IF1);
        next_sym();
        x->o1 = paren_expr();
        x->o2 = statement();
        if( sym == ELSE_SYM ) { // ... "else" <statement>
            x->kind = IF2;
            next_sym();
            x->o3 = statement();
        }

    } else if( sym == WHILE_SYM ) {  // "while" <paren_expr> <statement>
        x = new_node(WHILE);
        next_sym();
        x->o1 = paren_expr();
        x->o2 = statement();

    } else if( sym == DO_SYM ) {  // "do" <statement> "while" <paren_expr> ";"
        x = new_node(DO);
        next_sym();
        x->o1 = statement();
        if( sym == WHILE_SYM ) next_sym();
        else syntax_error();
        x->o2 = paren_expr();
        if( sym == ';' ) next_sym();
        else syntax_error();

    } else if( sym == ';' ) { // ";"
        x = new_node(EMPTY);
        next_sym();

    } else if( sym == '{' ) {  // "{" { <statement> } "}"
        x = new_node(EMPTY);
        next_sym();
        while( sym != '}' ) {
            t = x;
            x = new_node( SEQ );
            x->o1 = t;
            x->o2 = statement();
        }
        next_sym();

    } else { // <expr> ";"
        x = new_node( EXPR );
        x->o1 = expr();
        if( sym == ';' ) next_sym();
        else syntax_error();
    }
    return x;
}

node *program() { // <program> ::= <statement>
    node *x = new_node(PROG);
    next_sym();
    x->o1 = statement();
    if( sym != EOI ) syntax_error();
    return x;
}

/* Code generator. */

enum {
    IFETCH,
    ISTORE,
    IPUSH,
    IPOP,
    IADD,
    ISUB,
    ILT,
    JZ,
    JNZ,
    JMP,
    HALT
};

typedef char code;
code object[1000], *here = object;

void g( code c ) {
    *here++ = c;
} /* missing overflow check */

code *hole() {
    return here++;
}
void fix(code *src, code *dst) {
    *src = dst-src;
} /* missing overflow check */

void c(node *x) {
    code *p1, *p2;
    switch( x->kind ) {
        case VAR  : g(IFETCH); g(x->val); break;
        case CST  : g(IPUSH); g(x->val); break;
        case ADD  : c(x->o1); c(x->o2); g(IADD); break;
        case SUB  : c(x->o1); c(x->o2); g(ISUB); break;
        case LT   : c(x->o1); c(x->o2); g(ILT); break;
        case SET  : c(x->o2); g(ISTORE); g(x->o1->val); break;
        case IF1  : c(x->o1); g(JZ); p1=hole(); c(x->o2); fix(p1,here); break;
        case IF2  : c(x->o1); g(JZ); p1=hole(); c(x->o2); g(JMP); p2=hole();
                  fix(p1,here); c(x->o3); fix(p2,here); break;
        case WHILE: p1=here; c(x->o1); g(JZ); p2=hole(); c(x->o2);
                  g(JMP); fix(hole(),p1); fix(p2,here); break;
        case DO   : p1=here; c(x->o1); c(x->o2); g(JNZ); fix(hole(),p1); break;
        case EMPTY: break;
        case SEQ  : c(x->o1); c(x->o2); break;
        case EXPR : c(x->o1); g(IPOP); break;
        case PROG : c(x->o1); g(HALT); break;
    }
}

//
// Virtual machine.
//
int globals[26];

void run() {
    int stack[1000], *sp = stack;
    code *pc = object;
    again: switch (*pc++) {
        case IFETCH: *sp++ = globals[*pc++];               goto again;
        case ISTORE: globals[*pc++] = sp[-1];              goto again;
        case IPUSH : *sp++ = *pc++;                        goto again;
        case IPOP  : --sp;                                 goto again;
        case IADD  : sp[-2] = sp[-2] + sp[-1]; --sp;       goto again;
        case ISUB  : sp[-2] = sp[-2] - sp[-1]; --sp;       goto again;
        case ILT   : sp[-2] = sp[-2] < sp[-1]; --sp;       goto again;
        case JMP   : pc += *pc;                            goto again;
        case JZ    : if (*--sp == 0) pc += *pc; else pc++; goto again;
        case JNZ   : if (*--sp != 0) pc += *pc; else pc++; goto again;
    }
}

//
// Main program.
//
int main() {
    int i;

    fin = fopen( "script.lys", "r" );
    c(program());

    for( i=0; i<26; i++ ) globals[i] = 0;
    run();
    for( i=0; i<26; i++ ) {
        if( globals[i] != 0 )
            printf( "%c = %d\n", 'a'+i, globals[i] );
    }

  return 0;
}
