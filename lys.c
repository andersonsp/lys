#include "lys.h"

// * Interpreter for Lys scripting language.
// * BUILD: gcc lys.c -o lys
// * USAGE: lys file.lys
// *
// * The grammar of Lys in EBNF is:
// *
// * exp        ::= { message | terminator }
// * message    ::= symbol [arguments]
// * arguments  ::= "(" [exp [ { "," exp } ]] ")"
// * symbol     ::= identifier | number | string
// * terminator ::= "\n" | ";"

//
// Object defs
//
#define MCACHE 1    /* nonzero to enable global method cache        */
#define POINTER_INT long
#define POINTER_UINT unsigned long

typedef struct _Vtable Vtable;
typedef struct _Object Object;
typedef struct _Closure Closure;
typedef struct _Symbol Symbol;

typedef Object *(*imp_t)(Closure *closure, Object *receiver, ...);

struct _Vtable {
  Vtable  *_vt[0];
  int size, tally;
  Object **keys, **values;
  Vtable  *parent;
};

struct _Object {
  Vtable *_vt[0];
};

struct _Closure {
  Vtable *_vt[0];
  imp_t      method;
  Object *data;
};

struct _Symbol {
  Vtable *_vt[0];
  char *string;
};

Vtable *SymbolList= NULL;

Vtable *vtable_vt;
Vtable *object_vt;
Vtable *symbol_vt;
Vtable *closure_vt;

Object *s_add_method = NULL;
Object *s_allocate  = NULL;
Object *s_delegated = NULL;
Object *s_lookup    = NULL;

inline void *alloc(size_t size) {
  Vtable **ppvt= (Vtable **)calloc(1, sizeof(Vtable *) + size);
  return (void *)(ppvt + 1);
}

Object *symbol_new(char *string) {
  Symbol *symbol = (Symbol *)alloc(sizeof(Symbol));
  symbol->_vt[-1] = symbol_vt;
  symbol->string = strdup(string);
  return (Object *)symbol;
}

Object *closure_new(imp_t method, Object *data) {
  Closure *closure = (Closure *)alloc(sizeof(Closure));
  closure->_vt[-1] = closure_vt;
  closure->method  = method;
  closure->data    = data;
  return (Object *)closure;
}

Object *vtable_lookup(Closure *closure, Vtable *self, Object *key);

# define send(RCV, MSG, ARGS...) ({             \
      Object   *r = (Object *)(RCV);   \
      static Vtable   *prevVT  = NULL;          \
      static Closure  *closure = NULL;          \
      register Vtable *thisVT  = r->_vt[-1];     \
      thisVT == prevVT ? closure : (prevVT  = thisVT, closure = bind(r, (MSG)));               \
      closure->method(closure, r, ##ARGS);          \
    })

#if MCACHE
struct entry {
  Vtable  *vtable;
  Object  *selector;
  Closure *closure;
} MethodCache[8192];
#endif

Closure *bind(Object *rcv, Object *msg) {
  Closure *c;
  Vtable  *vt = rcv->_vt[-1];
#if MCACHE
  struct entry   *cl = MethodCache + ((((POINTER_UINT)vt << 2) ^ ((POINTER_UINT)msg >> 3)) & ((sizeof(MethodCache) / sizeof(struct entry)) - 1));
  if (cl->vtable == vt && cl->selector == msg) return cl->closure;
#endif
  c = ((msg == s_lookup) && (rcv == (Object *)vtable_vt))
    ? (Closure *)vtable_lookup(0, vt, msg)
    : (Closure *)send(vt, s_lookup, msg);
#if MCACHE
  cl->vtable   = vt;
  cl->selector = msg;
  cl->closure  = c;
#endif
  return c;
}

Vtable *vtable_delegated(Closure *closure, Vtable *self) {
    Vtable *child= (Vtable *)alloc(sizeof(Vtable));
    child->_vt[-1] = self ? self->_vt[-1] : 0;
    child->size    = 2;
    child->tally   = 0;
    child->keys    = (Object **)calloc(child->size, sizeof(Object *));
    child->values  = (Object **)calloc(child->size, sizeof(Object *));
    child->parent  = self;
    return child;
}

Object *vtable_allocate(Closure *closure, Vtable *self, int payloadSize) {
  Object *object = (Object *)alloc(payloadSize);
  object->_vt[-1] = self;
  return object;
}

imp_t vtable_add_method(Closure *closure, Vtable *self, Object *key, imp_t method) {
    int i;
    for( i = 0; i < self->tally; ++i ) {
        if( key == self->keys[i] ) return ((Closure *)self->values[i])->method = method;
    }
    if( self->tally == self->size ) {
        self->size  *= 2;
        self->keys   = (Object **)realloc(self->keys,   sizeof(Object *) * self->size);
        self->values = (Object **)realloc(self->values, sizeof(Object *) * self->size);
    }
    self->keys[ self->tally ] = key;
    self->values[ self->tally++ ] = closure_new(method, 0);
    return method;
}

Object *vtable_lookup(Closure *closure, Vtable *self, Object *key) {
    int i;
    for( i = 0;  i < self->tally; ++i) {
        if (key == self->keys[i]) return self->values[i];
    }
    if( self->parent ) return send(self->parent, s_lookup, key);
    fprintf(stderr, "lookup failed %p %s\n", self, ((Symbol *)key)->string);
    return 0;
}

Object *symbol_intern(Closure *closure, Object *self, char *string) {
    Object *symbol;
    int i;
    for(i = 0;  i < SymbolList->tally;  ++i) {
        symbol = SymbolList->keys[i];
        if(!strcmp(string, ((Symbol *)symbol)->string)) return symbol;
    }
    symbol = symbol_new(string);
    vtable_add_method(0, SymbolList, symbol, 0);
    return symbol;
}

void init(void) {
    vtable_vt = vtable_delegated(0, 0);
    vtable_vt->_vt[-1] = vtable_vt;

    object_vt = vtable_delegated(0, 0);
    object_vt->_vt[-1] = vtable_vt;
    vtable_vt->parent = object_vt;

    symbol_vt  = vtable_delegated(0, object_vt);
    closure_vt = vtable_delegated(0, object_vt);

    SymbolList = vtable_delegated(0, 0);

    s_lookup = symbol_intern(0, 0, "lookup");
    s_add_method = symbol_intern(0, 0, "add_method");
    s_allocate = symbol_intern(0, 0, "allocate");
    s_delegated = symbol_intern(0, 0, "delegated");

    vtable_add_method(0, vtable_vt, s_lookup, (imp_t)vtable_lookup);
    vtable_add_method(0, vtable_vt, s_add_method, (imp_t)vtable_add_method);

    send(vtable_vt, s_add_method, s_allocate, vtable_allocate);
    send(vtable_vt, s_add_method, s_delegated, vtable_delegated);
}

//----------------------------------------------------------------

Object *s_new= 0;
Object *s_length= 0;

struct Number {
  Vtable *_vt[0];
};

Vtable *Number_vt = 0;
Object *Number = 0;

Object *Number_new(Closure *closure, struct Number *self) {
  fprintf(stderr, "Number_new\n");
  exit(1);
  return 0;
}

int Number_length(Closure *closure, struct Number *self) {
  fprintf(stderr, "Number has no length\n");
  exit(1);
  return 0;
}

struct String {
  Vtable *_vt[0];
  int length;
  char *chars;
};

Vtable *String_vt = 0;
Object *String = 0;

Object *String_new(Closure *closure, struct String *self, int size) {
  struct String *clone = (struct String *)send(self->_vt[-1], s_allocate, sizeof(struct String));
  clone->length = size;
  clone->chars  = (char *)malloc(size);
  return (Object *)clone;
}

int String_length(Closure *closure, struct String *self) {
  return self->length;
}

struct Vector {
  Vtable *_vt[0];
  int length;
  Object *contents;
};

Vtable *Vector_vt = 0;
Object *Vector = 0;

Object *Vector_new(Closure *closure, struct Vector *self, int size) {
  struct Vector *clone = (struct Vector *)send(self->_vt[-1], s_allocate, sizeof(struct Vector));
  clone->length   = size;
  clone->contents = (Object *)calloc(size, sizeof(Object *));
  return (Object *)clone;
}

int Vector_length(Closure *closure, struct Vector *self) {
  return self->length;
}

void init2(void) {
  s_new    = symbol_intern(0, 0, "new");
  s_length = symbol_intern(0, 0, "length");

  Number_vt = (Vtable *)send(object_vt, s_delegated);
  String_vt = (Vtable *)send(object_vt, s_delegated);
  Vector_vt = (Vtable *)send(object_vt, s_delegated);

  send(Number_vt, s_add_method, s_new, Number_new);
  send(String_vt, s_add_method, s_new, String_new);
  send(Vector_vt, s_add_method, s_new, Vector_new);

  send(Number_vt, s_add_method, s_length, Number_length);
  send(String_vt, s_add_method, s_length, String_length);
  send(Vector_vt, s_add_method, s_length, Vector_length);

  Number = send(Number_vt, s_allocate, 0);
  String = send(String_vt, s_allocate, 0);
  Vector = send(Vector_vt, s_allocate, 0);
}

//----------------------------------------------------------------

void doit(void) {
    int i, j;

    Object *a = send(String, s_new, 1);
    Object *b = send(Vector, s_new, 3);

    for( i = 0, j = 0;  i < 200;  ++i ) {
        j += (POINTER_INT)send(a, s_length) + (POINTER_INT)send(b, s_length);
    }

    printf("total %d\n", j);
}

int main() {
  init();
  init2();
  doit();
  return 0;
}

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
    again: switch(ch) {
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

