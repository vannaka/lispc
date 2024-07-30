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
    size_t count;
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

/*--------------------------------------------------
Result stuff
--------------------------------------------------*/
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

/*--------------------------------------------------
LVal stuff
--------------------------------------------------*/
void lval_print(lval_t* v);
Result lval_eval_sexpr(lval_t* v);


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

void lval_add(lval_t* head, lval_t* chld) {
    head->count++;
    head->cell = realloc(head->cell, sizeof(lval_t*) * head->count);
    head->cell[head->count-1] = chld;
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
        for (size_t i = 0; i < v->count; i++) {
            lval_delete(v->cell[i]);
        }
        /* Delete list head */
        free(v->cell);

        break;
    }

    /* delete lval */
    free(v);
}

Result parse_number(char* str) {
    long accum;
    char* endptr;
    Result res;

    accum = strtol(str, &endptr, 10);

    // NaN
    if (endptr == str) {
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

/**
 * @brief Convert AST structure to S-Expression structure.
 * 
 * @param[in]   a 
 * @return      Result
 */
Result ast_to_lval(mpc_ast_t* a) {
    #define DONT_CARE "DONT_CARE"
    static const Result signaling_error = {
        .variant = Result__Err,
        .err = DONT_CARE,
    };

    Result res;
    lval_t* lval;

    // Number or Symbol
    if (strstr(a->tag, "number")) return parse_number(a->contents);
    if (strstr(a->tag, "symbol")) return Ok(lval_sym(a->contents));

    // Ignore all other nodes which aren't S-Expressions
    if (NULL == strstr(a->tag, ">")) {
        return signaling_error;
    }
    
    lval = lval_sexpr();

    // Process s-expression's children
    for (int i = 0; i < a->children_num; i++) {
        res = ast_to_lval(a->children[i]);
        if (isErr(&res)) {
            if (strcmp(res.err, DONT_CARE) == 0) {
                continue;
            }
            else {
                /* Cleanup any alloc'd lvals on error */
                lval_delete(lval);
                return res;
            }
        }

        lval_add(lval, res.lval);
    }

    return Ok(lval);

    #undef DONT_CARE
}

void lval_expr_print(lval_t* v, char l, char r) {
    putchar(l);
    
    for (size_t i = 0; i < v->count; i++) {
        lval_print(v->cell[i]);

        if (i != v->count-1) 
            putchar(' ');
    }

    putchar(r);
}

/**
 * @brief Print S-Expression
 * 
 * @param sexpr
 */
void lval_print(lval_t* v) {
    switch (v->type) {
        case Number: printf("%li", v->num); break;
        case Symbol: printf("%s", v->sym); break;
        case SExpression: lval_expr_print(v, '(', ')'); break;
    }
}

/**
 * @brief Print S-Expression with a new line.
 * 
 * @param v 
 */
void lval_println(lval_t* v) {
    lval_print(v);
    putchar('\n');
}

/**
 * @brief Print an error message and free the string.
 * 
 * @param e String to print then free.
 */
void err_print(char* e) {
    fprintf(stderr, "Error: %s\n", e);
    free(e);
}


lval_t* lval_pop(lval_t* v, size_t i) {
    lval_t* x;

    assert(i <= v->count);

    // Save off value at i
    x = v->cell[i];

    // Shift over cells to remove slot i.
    memmove(&v->cell[i], &v->cell[i+1], sizeof(lval_t*) * (v->count-i-1));

    v->count--;

    v->cell = realloc(v->cell, sizeof(lval_t*) * v->count);
    return x;
}

/**
 * @brief Removes and returns an item from the S-Expression while freeing
 *  memory for the rest of the S-Expression.
 * 
 * @param v 
 * @param i 
 * @return lval_t* 
 */
lval_t* lval_take(lval_t* v, size_t i) {
    lval_t* x = lval_pop(v, i);
    lval_delete(v);
    return x;
}


/**
 * @brief Performs operation and returns result.
 *
 * On error Err is returned. 
 * 
 * @param op 
 * @param l 
 * @param r 
 * @return Result 
 */
Result builtin_op( lval_t* a, char* op ) {
    Result res;
    lval_t* v;
    long accum;

    /* Accumulate operands w/ given op */
    for (size_t i = 0; i < a->count; i++ ) {
        v = a->cell[i];

        /* Validate operand */
        if (Number != v->type) {
            res = Err("Cannot operate on non-number!");
            goto finish_op;
        }

        /* 1st operand only inits accumulator */
        if (0 == i) {
            accum = v->num;
            continue;
        }

        if (strcmp(op, "+") == 0) {
            accum += v->num;
        }
        else if (strcmp(op, "-") == 0) {
            /* unary negation */
            if (v->count == 1) 
                accum = -accum;
            /* normal subtraction */
            else 
                accum -= v->num;
        }
        else if (strcmp(op, "*") == 0) {
            accum *= v->num;
        }
        else if (strcmp(op, "/") == 0) {
            /* Check for div by zero */
            if (0 == v->num) {
                res = Err("DivByZero");
                goto finish_op;
            }
            /* normal division */
            else {
                accum /= v->num; 
            }
        }
        else {
            res = Err("BadOp");
            goto finish_op;
        }
    }

    res = Ok(lval_num(accum));

finish_op:
    /* consume lval */
    lval_delete(a);

    return res;
}

/**
 * @brief Evaluate an Expression
 * 
 * @param v 
 * @return Result 
 */

Result lval_eval(lval_t* v) {
    if (SExpression == v->type) 
        return lval_eval_sexpr(v);
    else
        return Ok(v);
}


/**
 * @brief Evaluate an S-Expression
 * 
 * @param v 
 * @return Result 
 */
Result lval_eval_sexpr(lval_t* v) {
    Result res;

    /* Evaluate children */
    for (size_t i = 0; i < v->count; i++) {
        res = lval_eval(v->cell[i]);

        if (isErr(&res)) {
            return res;
        }

        v->cell[i] = res.lval;
    }

    /* Empty expression */
    if (0 == v->count) return Ok(v);

    /* Single expression */
    if (1 == v->count) return Ok(lval_take(v, 0));

    /* Ensure first entry is a symbol */
    lval_t* f = lval_pop(v, 0);
    if (Symbol != f->type) {
        lval_delete(f);
        lval_delete(v);
        return Err("S-expression Does not start with symbol!");
    }

    /* Call builtin with operator */
    res = builtin_op(v, f->sym);
    lval_delete(f);
    return res;
}


int main(void) {
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

            /* Parse AST */
            res = ast_to_lval(r.output);
            if (isErr(&res)) {
                err_print(res.err);
            } else {
                // lval_println(res.lval);

                /* Evaluate expression */
                res = lval_eval(res.lval);
                if (isErr(&res)) {
                    err_print(res.err);
                } else {
                    lval_println(res.lval);
                }
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