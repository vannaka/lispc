/*
* main.c
*
* Lispy
*
*/

/***************************************************************
* INCLUDES
***************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <limits.h>

#if _WIN32
#include <string.h>

static char buffer[2048];

/* fake readline function */
char* readline(char* prompt) {
    fputs(prompt, stdout);
    fgets(buffer, sizeof buffer, stdin);
    char* cpy = malloc(strlen(buffer)+1);
    strcpy(cpy, buffer);
    cpy[strlen(cpy)-1] = '\0';
    return cpy;
}

void add_history(char* unused) {}

#else
#include <editline/readline.h>
#include <editline/history.h>
#endif

#include "mpc.h"

/***************************************************************
* MACROS
***************************************************************/
#define stringify(s) #s
#define cnt_of_array(a) sizeof((a))/sizeof((a)[0])

/***************************************************************
* TYPES
***************************************************************/

enum lval_variant {
    Number,
    Symbol,
    SExpression
};

typedef struct lval_t lval_t;
typedef struct lval_t
{
    enum lval_variant type;
    union {
        long num;
        char* sym;
    };

    // List of lval_t*
    lval_t** cell;
    int count;
} lval_t;

enum ResultVariant {
    Result__Ok,
    Result__Err
};

typedef struct
{
    enum ResultVariant variant;
    union {
        lval_t* lval;   // Result::Ok value.
        char* err;      // Result::Err value.
    };
} Result;


/***************************************************************
* FUNCTIONS
***************************************************************/

Result Ok(lval_t* lval) {
    Result res;
    res.variant = Result__Ok;
    res.lval = lval;
    return res;
}

Result Err(char* m) {
    Result res;
    res.variant = Result__Err;
    res.err = malloc(strlen(m) + 1);
    strcpy(res.err, m);
    return res;
}

bool isErr(Result *res) {
    return res->variant == Result__Err;
}

bool isOk(Result *res) {
    return res->variant == Result__Ok;
}

lval_t* lval_num(long num) {
    lval_t* lval = malloc(sizeof(lval_t));
    lval->type = Number;
    lval->num = num;
    return lval;
}

lval_t* lval_sym(char* s) {
    lval_t* v = malloc(sizeof(lval_t));
    v->type = Symbol;
    v->sym = malloc(strlen(s) + 1);
    strcpy(v->sym, s);
    return v;
}

lval_t* lval_sexpr(void) {
  lval_t* v = malloc(sizeof(lval_t));
  v->type = SExpression;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lval_delete(lval_t* v) {
    switch (v->type)
    {
    case Number: /* Nothing to do */ break;
    case Symbol: free(v->sym); break;
    case SExpression:
        /* Delete all elements in list */
        for (int i = 0; i < v->count; i++) {
            lval_delete(v->cell[i]);
        }
        /* Delete list head */
        free(v->cell);

        break;
    }

    /* delete lval */
    free(v);
}

Result do_op( char* op, long l, long r) {
    Result res;
    
    switch (*op)
        {
        case '+': res = Ok( lval_num(l + r) ); break;
        case '-': res = Ok( lval_num(l - r) ); break;
        case '*': res = Ok( lval_num(l * r) ); break;
        case '/': {
            // Check for div by zero
            if (0 == r) {
                res = Err("DivByZero");
            } else {
                res = Ok( lval_num(l / r) );
            }
            break;
        }
        default:
            res = Err("BadOp");
        }

    return res;
}

Result evaluate(mpc_ast_t* a) {
    long accum;
    char* op, *endptr;
    Result res;

    // Base case: number
    if( strstr(a->tag, "number")) {
        accum = strtol(a->contents, &endptr, 10);

        // NaN
        if (endptr == a->contents) {
            res = Err("BadNum_NaN");
        } 
        // Range
        else if ( (accum == LONG_MAX || accum == LONG_MIN) 
               && (ERANGE == errno                       ) ) {
            res = Err("BadNum_Range");
        } 
        // OK
        else {
            res = Ok(lval_num(accum));
        }

        return res;
    }

    // Get operator char. Will always be 2nd child
    op = a->children[1]->contents;
    
    // Evaluate 1st operand
    res = evaluate(a->children[2]);

    if (isErr(&res)) return res;

    accum = res.lval->num;

    // Evaluate remaining opperands
    for (int i = 3; i < a->children_num - 1; i++) {
        res = evaluate(a->children[i]);
        if (isErr(&res)) return res;

        res = do_op(op, accum, res.lval->num);
        if (isErr(&res)) return res;

        accum = res.lval->num;
    }

    res.lval->num = accum;

    return res;
}

int main(int argc, char** argv) {
    Result res;

    /* Setup grammer for Polish Notation */
    mpc_parser_t* Number = mpc_new("number");
    mpc_parser_t* Symbol = mpc_new("symbol");
    mpc_parser_t* Sexpr  = mpc_new("sexpr");
    mpc_parser_t* Expr   = mpc_new("expr");
    mpc_parser_t* Lispy  = mpc_new("lispy");

    mpca_lang(MPCA_LANG_DEFAULT,
        "                                        \
        number : /-?[0-9]+/ ;                    \
        symbol : '+' | '-' | '*' | '/' ;         \
        sexpr  : '(' <expr>* ')' ;               \
        expr   : <number> | <symbol> | <sexpr> ; \
        lispy  : /^/ <expr>* /$/ ;               \
        ",
    Number, Symbol, Sexpr, Expr, Lispy);

    /* Print Version and Exit Information */
    puts("Lispy Version 0.0.0.0.1");
    puts("Press Ctrl+c to Exit\n");

    /* In a never ending loop */
    while (1) {

        /* Output our prompt and get input */
        char* input = readline("lispy> ");
        add_history(input);

        /* parser input string */
        mpc_result_t r;
        if (mpc_parse("<stdin>", input, Lispy, &r)) {
            /* Success: Print the AST */
            // mpc_ast_print(r.output);

            res = evaluate(r.output);
            if (isOk(&res)) {
                printf("%li\n", res.lval->num);
            } else {
                fprintf(stderr, "Error: %s", res.err);
            }

            mpc_ast_delete(r.output);
        } else {
            /* failure: Print the error */
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
        }
        
        /* cleanup input buffer */
        free(input);
    }

    /* cleanup */
    mpc_cleanup(5, Number, Symbol, Sexpr, Expr, Lispy);

    return 0;
}