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
#define UNUSED(x) (void)(x)

#define LASSERT(args, cond, fmt, ...)               \
    if (!(cond)) {                                  \
        Result res = printErr(fmt, ##__VA_ARGS__);  \
        lval_delete(args);                          \
        return res;                                 \
    }


/***************************************************************
* TYPES
***************************************************************/

/* Forward declarations */
typedef struct lval_t lval_t;
typedef struct lenv_t lenv_t;


/*------------------------------------------
Error handling types 
------------------------------------------*/
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


/*------------------------------------------
lval types 
------------------------------------------*/
enum lval_variant {
    Number,
    Symbol,
    Func,
    SExpression,
    QExpression
};

typedef Result(lbuiltin)(lenv_t*, lval_t*);

struct lval_t
{
    enum lval_variant type;
    union {
        long num;
        char* sym;
        lbuiltin* func;
    };

    // List of lval_t*
    lval_t** cell;
    size_t count;
};

struct lenv_t {
    /* Simple mapping from symbol to lval_t */
    size_t count;
    char** syms;
    lval_t** vals;
};


/***************************************************************
* FUNCTIONS
***************************************************************/

void lval_print(lval_t* v);
Result lval_eval(lenv_t* e, lval_t * v );
Result lval_eval_sexpr(lenv_t* e, lval_t* v);


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

Result printErr(char* fmt, ...) {
    va_list args, args2;
    va_start(args, fmt);
    va_copy(args2, args);

    size_t sz = vsnprintf(NULL, 0, fmt, args) + 1;
    char *err = malloc(sz);
    vsnprintf(err, sz, fmt, args2);

    va_end(args);
    va_end(args2);

    return Err(err);
}

/*--------------------------------------------------
lval_t constructors and destructors
--------------------------------------------------*/

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

lval_t* lval_func(lbuiltin* func) {
    lval_t* v = malloc(sizeof(lval_t));
    v->type = Func;
    v->func = func;
    return v;
}

lval_t* lval_add(lval_t* head, lval_t* chld) {
    head->count++;
    head->cell = realloc(head->cell, sizeof(lval_t*) * head->count);
    head->cell[head->count-1] = chld;
    return head;
}

lval_t* lval_sexpr(void) {
  lval_t* v = malloc(sizeof(lval_t));
  v->type = SExpression;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lval_t* lval_qexpr(void) {
  lval_t* v = malloc(sizeof(lval_t));
  v->type = QExpression;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lval_delete(lval_t* v) {
    switch (v->type)
    {
    case Number: /* Nothing to do */ break;
    case Symbol: free(v->sym); break;
    case Func: /* Nothing to do */ break;
    case SExpression: /* intentional fall-through */
    case QExpression:
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

lval_t* lval_copy(lval_t* v) {
    lval_t* x = malloc(sizeof(lval_t));
    x->type = v->type;

    switch (v->type)
    {
    /* Direct copies */
    case Func: x->func = v->func; break;
    case Number: x->num = v->num; break;

    /* String copies */
    case Symbol:
        x->sym = malloc(strlen(v->sym));
        strcpy(x->sym, v->sym);
        break;

    /* List copies */
    case SExpression:
    case QExpression:
        x->count = v->count;
        x->cell = malloc(sizeof(lval_t*) * v->count);
        for (size_t i = 0; i < v->count; i++) {
            /* Do a deep copy */
            x->cell[i] = lval_copy(v->cell[i]);
        }

        break;
    }

    return x;
}

char* str_type(int t) {
    switch(t) {
        case Func:          return "Function";
        case Number:        return "Number";
        case Symbol:        return "Symbol";
        case SExpression:   return "S-Expression";
        case QExpression:   return "Q-Expression";
        default:            return "Unknown";
    }
}


/*--------------------------------------------------
lenv_t constructors and destructors
--------------------------------------------------*/
lenv_t* lenv_new(void) {
    lenv_t* e = malloc(sizeof(lenv_t));
    e->count = 0;
    e->syms = NULL;
    e->vals = NULL;
    return e;
}

void lenv_del(lenv_t* e) {
    for (size_t i = 0; i < e->count; i++) {
        free(e->syms[i]);
        lval_delete(e->vals[i]);
    }

    free(e->syms);
    free(e->vals);
    free(e);
}

Result lenv_get(lenv_t* e, lval_t* v) {
    assert(Symbol == v->type);

    for (size_t i = 0; i < e->count; i++) {
        if(0 == strcmp(e->syms[i], v->sym)) {
            return Ok(lval_copy(e->vals[i]));
        }
    }

    return printErr("unbounded symbol: %s", v->sym);
}

/**
 * @brief Add a value for the given key to the given environment.
 * 
 * @param e Environment
 * @param k Key
 * @param v Value
 */
void lenv_put(lenv_t* e, lval_t* k, lval_t* v) {
    /* Replace existing variable with new one */
    for (size_t i = 0; i < e->count; i++) {
        if(0 == strcmp(e->syms[i], k->sym)) {
            lval_delete(e->vals[i]);
            e->vals[i] = lval_copy(v);
            return;
        }
    }

    /* alloc space for new entry */
    e->vals = realloc(e->vals, sizeof(lval_t*) * (e->count+1));
    e->syms = realloc(e->syms, sizeof(char*) * (e->count+1));

    /* add v to list */
    e->vals[e->count] = lval_copy(v);
    e->syms[e->count] = malloc(strlen(k->sym)+1);
    strcpy(e->syms[e->count], k->sym);
    
    e->count++;
}


/*--------------------------------------------------
Functions to print things
--------------------------------------------------*/

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
        case Func: printf("<function>"); break;
        case SExpression: lval_expr_print(v, '(', ')'); break;
        case QExpression: lval_expr_print(v, '{', '}'); break;
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


/*--------------------------------------------------
AST Parsing functions
--------------------------------------------------*/

Result parse_number(char* str) {
    long accum;
    char* endptr;
    Result res;

    accum = strtol(str, &endptr, 10);

    // NaN
    if (endptr == str) {
        res = printErr("BadNum_NaN: %s", str);
    } 
    // Range
    else if ( (accum == LONG_MAX || accum == LONG_MIN) 
            && (ERANGE == errno                       ) ) {
        res = printErr("BadNum_Range: %s", str);
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
    #define DONT_CARE "!!INTERNAL_SIGNALING_ERROR!!"
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
    
    if (strstr(a->tag, "qexpr")) {
        lval = lval_qexpr();
    } else {
        lval = lval_sexpr();
    }

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


/*--------------------------------------------------
Evaluation functions
--------------------------------------------------*/

/**
 * @brief Removes and returns an item from the S-Expression.
 * 
 * @param v 
 * @param i 
 * @return lval_t* 
 */
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
 * @brief Removes and returns an item from the S-Expression and
 *  deletes the rest of the S-Expression.
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
 * @brief Takes the children from y and appends them to x, then
 *  deletes y.
 * 
 * @param x 
 * @param y 
 * @return lval_t* 
 */
lval_t* lval_join(lval_t* x, lval_t* y) {
    while (y->count) {
        x = lval_add(x, lval_pop(y, 0));
    }

    lval_delete(y);
    return x;
}


/**
 * @brief Performs operation on S-Expression and return result. The given
 *  lval_t is consumed.
 *
 * On error Err is returned. 
 * 
 * @param op 
 * @param l 
 * @param r 
 * @return Result 
 */
Result builtin_op(lenv_t* e, lval_t* a, char* op ) {
    UNUSED(e);
    Result res;
    lval_t* v;
    long accum;

    /* Accumulate operands w/ given op */
    for (size_t i = 0; i < a->count; i++ ) {
        v = a->cell[i];

        /* Validate operand */
        if (Number != v->type) {
            res = printErr(
                    "Function '%s' passed incorrect type for argument %d. Act: %s, Exp: %s", 
                    op, 
                    str_type(v->type), 
                    str_type(Number));
            
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
            res = printErr("BadOp: %s", op);
            goto finish_op;
        }
    }

    res = Ok(lval_num(accum));

finish_op:
    /* consume lval */
    lval_delete(a);

    return res;
} /* builtin_op() */


Result builtin_add(lenv_t* e, lval_t* a) {
  return builtin_op(e, a, "+");
}

Result builtin_sub(lenv_t* e, lval_t* a) {
  return builtin_op(e, a, "-");
}

Result builtin_mul(lenv_t* e, lval_t* a) {
  return builtin_op(e, a, "*");
}

Result builtin_div(lenv_t* e, lval_t* a) {
  return builtin_op(e, a, "/");
}


/**
 * @brief Takes a Q-Expression and returns a Q-Expression with
 *  only the first element. Consumes the given lval.
 * 
 * @param a Argument to 'head'.
 * @return Result
 */
Result builtin_head(lenv_t* e, lval_t* a) {
    UNUSED(e);
    lval_t* v;

    LASSERT(a, 1 == a->count,
        "Func 'head' passed too many arguments. Act: %d, Exp: %d", a->count, 1);
    LASSERT(a, QExpression == a->cell[0]->type,
        "Function 'head' passed incorrect type for argument 0. "
        "Got %s, Expected %s.",
        str_type(a->cell[0]->type), str_type(QExpression));
    LASSERT(a, 0 != a->cell[0]->count,
        "List passed to 'head' is empty!");

    v = lval_take(a, 0);
    while (v->count > 1) { lval_delete(lval_pop(v, 1)); }
    
    return Ok(v);
} /* builtin_head() */


/**
 * @brief Takes a Q-Expression and returns a Q-Expression with
 *  the first element removed.
 * 
 * @param a Argument to 'tail'
 * @return Result
 */
Result builtin_tail(lenv_t* e, lval_t* a) {
    UNUSED(e);
    lval_t* v;

    LASSERT(a, 1 == a->count,
        "Func 'tail' passed too many arguments. Act: %d, Exp: %d", a->count, 1);
    LASSERT(a, QExpression == a->cell[0]->type,
        "Function 'tail' passed incorrect type for argument 0. "
        "Got %s, Expected %s.",
        str_type(a->cell[0]->type), str_type(QExpression));
    LASSERT(a, 0 != a->cell[0]->count,
        "Func 'tail' passed empty list!");

    v = lval_take(a, 0);
    lval_delete(lval_pop(v, 0));
    
    return Ok(v);
} /* builtin_tail() */


/**
 * @brief Converts a given S-Expression to a Q-Expression
 * 
 * @param v Argument to 'list'.
 * @return Result 
 */
Result builtin_list(lenv_t* e, lval_t* a) {
    UNUSED(e);
    a->type = QExpression;
    return Ok(a);
}


/**
 * @brief Takes a Q-Expression and evaluates it as if it were
 *  an S-Expression.
 * 
 * @param a Argument to 'eval'
 * @return Result 
 */
Result builtin_eval(lenv_t* e, lval_t* a) {
    lval_t* v;

    LASSERT(a, 1 == a->count,
        "Func 'eval' passed too many arguments. Act: %d, Exp: %d", a->count, 1);
    LASSERT(a, QExpression == a->cell[0]->type,
        "Function 'eval' passed incorrect type for argument 0. "
        "Got %s, Expected %s.",
        str_type(a->cell[0]->type), str_type(QExpression));

    v = lval_take(a, 0);
    v->type = SExpression;
    return lval_eval(e, v);

} /* builtin_eval() */


/**
 * @brief Take one or more Q-Expressions and join them together.
 * 
 * @param a Arguments to 'join'
 * @return Result
 */
Result builtin_join(lenv_t* e, lval_t* a) {
    UNUSED(e);

    /* Validate argument types */
    for (size_t i = 0; i < a->count; i++) {
        LASSERT(a, QExpression == a->cell[i]->type,
            "Function 'join' passed incorrect type for argument %i. "
            "Got %s, Expected %s.",
            i, str_type(a->cell[i]->type), str_type(QExpression));
    }

    /* grab the 1st argument */
    lval_t* x = lval_pop(a, 0);

    /* append every other argument's contents to the first */
    while (a->count) {
        x = lval_join(x, lval_pop(a, 0));
    }

    lval_delete(a);
    return Ok(x);
}

Result builtin_def(lenv_t* e, lval_t* a) {
    LASSERT(a, QExpression == a->cell[0]->type,
        "Function 'def' passed incorrect type for argument 0. "
        "Got %s, Expected %s.",
        str_type(a->cell[0]->type), str_type(QExpression));

    /* 1st arg is symbol list, remaining are values */
    lval_t* syms = a->cell[0];

    /* Validate all list elements are symbols */
    for (size_t i = 0; i < syms->count; i++) {
        LASSERT(a, syms->cell[i]->type == Symbol, "Function 'def' cannot define non-symbol");
    }

    /* Number of symbols must eq number of values */
    LASSERT(a, syms->count == a->count-1,
        "Function 'def', the number of symbols (%d) does not match the number of values (%d).", 
        syms->count, a->count-1);

    /* Assign k:v pairs to the environment */
    for (size_t i = 0; i < syms->count; i++) {
        lenv_put(e, syms->cell[i], a->cell[1+i]);
    }

    lval_delete(a);
    return Ok(lval_sexpr());
}


void lenv_add_builtin(lenv_t* e, char* name, lbuiltin func) {
    lval_t* k = lval_sym(name);
    lval_t* v = lval_func(func);
    lenv_put(e, k, v);
    lval_delete(k);
    lval_delete(v);
}

void lenv_add_builtins(lenv_t* e) {
    /* List Functions */
    lenv_add_builtin(e, "list", builtin_list);
    lenv_add_builtin(e, "head", builtin_head);
    lenv_add_builtin(e, "tail", builtin_tail);
    lenv_add_builtin(e, "eval", builtin_eval);
    lenv_add_builtin(e, "join", builtin_join);

    /* Mathematical Functions */
    lenv_add_builtin(e, "+", builtin_add);
    lenv_add_builtin(e, "-", builtin_sub);
    lenv_add_builtin(e, "*", builtin_mul);
    lenv_add_builtin(e, "/", builtin_div);

    /* Variable Functions */
    lenv_add_builtin(e, "def",  builtin_def);
}


/**
 * @brief Evaluate an Expression
 * 
 * @param v 
 * @return Result 
 */

Result lval_eval(lenv_t* e, lval_t* v) {
    if (SExpression == v->type) {
        return lval_eval_sexpr(e, v);
    }
    else if (Symbol == v->type) {
        /* Convert from Symbol to Func type */
        Result x = lenv_get(e, v);
        lval_delete(v);
        return x;
    }
    else {
        return Ok(v);
    }
} /* lval_eval() */


/**
 * @brief Evaluate an S-Expression
 * 
 * @param v 
 * @return Result 
 */
Result lval_eval_sexpr(lenv_t* e, lval_t* v) {
    Result res;

    /* Evaluate children */
    for (size_t i = 0; i < v->count; i++) {
        res = lval_eval(e, v->cell[i]);

        if (isErr(&res)) {
            lval_take(v, i);
            return res;
        }

        v->cell[i] = res.lval;
    }

    /* Empty expression */
    if (0 == v->count) return Ok(v);

    /* Single expression */
    if (1 == v->count) return Ok(lval_take(v, 0));

    /* Ensure first entry is a function */
    lval_t* f = lval_pop(v, 0);
    if (Func != f->type) {
        lval_delete(f);
        lval_delete(v);
        return Err("First element is not a function");
    }

    /* Call function */
    res = f->func(e, v);
    lval_delete(f);
    return res;
} /* lval_eval_sexpr() */


int main(void) {
    Result res;

    /* Setup grammer for Polish Notation */
    mpc_parser_t* Number = mpc_new("number");
    mpc_parser_t* Symbol = mpc_new("symbol");
    mpc_parser_t* Sexpr  = mpc_new("sexpr");
    mpc_parser_t* Qexpr  = mpc_new("qexpr");
    mpc_parser_t* Expr   = mpc_new("expr");
    mpc_parser_t* Lispy  = mpc_new("lispy");

    mpca_lang(MPCA_LANG_DEFAULT,
        "                                                  \
        number : /-?[0-9]+/ ;                              \
        symbol : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;        \
        sexpr  : '(' <expr>* ')' ;                         \
        qexpr  : '{' <expr>* '}' ;                         \
        expr   : <number> | <symbol> | <sexpr> | <qexpr> ; \
        lispy  : /^/ <expr>* /$/ ;                         \
        ",
    Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

    /* Print Version and Exit Information */
    puts("Lispy Version 0.0.0.0.1");
    puts("Press Ctrl+c to Exit\n");

    /* Setup environment */
    lenv_t* e = lenv_new();
    lenv_add_builtins(e);

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
                res = lval_eval(e, res.lval);
                if (isErr(&res)) {
                    err_print(res.err);
                } else {
                    lval_println(res.lval);
                    lval_delete(res.lval);
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
    lenv_del(e);
    mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

    return 0;
}