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
        lval_del(args);                             \
        return res;                                 \
    }

#define LASSERT_TYPE(name, a, num, t)                                   \
    LASSERT(a, (t) == (a)->cell[(num)]->type,                           \
        "Function '%s' passed incorrect type for argument %d. "         \
        "Got %s, Expected %s.",                                         \
        (name),                                                         \
        (num),                                                          \
        str_type((a)->cell[(num)]->type),                               \
        str_type((t)))

#define LASSERT_NUM(name, a, exp_num)                                   \
    LASSERT(a, (exp_num) == (a)->count,                                 \
        "Func '%s' passed too many arguments. Act: %d, Exp: %d",  \
        (name), (a)->count, (exp_num))


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
    String,
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
        char* str;
        struct {
            lbuiltin* builtin;
            lenv_t* env;
            lval_t* formals;
            lval_t* body;
        } func;
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
    lenv_t* par;
};


/***************************************************************
* GLOBAL VARIABLES
***************************************************************/
mpc_parser_t* Lispy;


/***************************************************************
* FUNCTIONS
***************************************************************/

void lval_print(lval_t* v);
void lval_print_str( lval_t * v );
Result lval_eval(lenv_t* e, lval_t * v );
Result lval_eval_sexpr(lenv_t* e, lval_t* v);
lenv_t* lenv_new(void);
void lenv_del(lenv_t* e);
Result lenv_get(lenv_t* e, lval_t* v);
void lenv_put(lenv_t* e, lval_t* k, lval_t* v);
lenv_t* lenv_copy(lenv_t* e);


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

lval_t* lval_str(char* s) {
    lval_t* v = lval_sym(s);
    v->type = String;
    return v;
}

lval_t* lval_func(lbuiltin* func) {
    lval_t* v = malloc(sizeof(lval_t));
    v->type = Func;
    v->func.builtin = func;
    return v;
}

lval_t* lval_lamda(lval_t* formals, lval_t* body) {
    lval_t* v = malloc(sizeof(lval_t));
    v->type = Func;
    v->func.builtin = NULL;
    v->func.env = lenv_new();
    v->func.formals = formals;
    v->func.body = body;
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

void lval_del(lval_t* v) {
    switch (v->type)
    {
    case Number: /* Nothing to do */ break;
    case Symbol: free(v->sym); break;
    case String: free(v->str); break;
    case Func:
        /* Clean up user defined function */
        if (!v->func.builtin) {
            lenv_del(v->func.env);
            lval_del(v->func.formals);
            lval_del(v->func.body);
        }
        break;
    case SExpression: /* intentional fall-through */
    case QExpression:
        /* Delete all elements in list */
        for (size_t i = 0; i < v->count; i++) {
            lval_del(v->cell[i]);
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
    case Func:
        if (v->func.builtin) {
            x->func.builtin = v->func.builtin;
        }
        else {
            x->func.builtin = NULL;
            x->func.env = lenv_copy(v->func.env);
            x->func.formals = lval_copy(v->func.formals);
            x->func.body = lval_copy(v->func.body);
        }

        break;
    case Number: x->num = v->num; break;

    /* String copies */
    case Symbol:
        x->sym = malloc(strlen(v->sym));
        strcpy(x->sym, v->sym);
        break;

    case String:
        x->str = malloc(strlen(v->str));
        strcpy(x->str, v->str);
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
        case String:        return "String";
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
    e->par  = NULL;
    return e;
}

void lenv_del(lenv_t* e) {
    for (size_t i = 0; i < e->count; i++) {
        free(e->syms[i]);
        lval_del(e->vals[i]);
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

    /* Search parent environment */
    if (e->par) {
        return lenv_get(e->par, v);
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
            lval_del(e->vals[i]);
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


/**
 * @brief Add a value for the given key to the global environment of
 *  the given environment.
 *
 * @param e Environment
 * @param k Key
 * @param v Value
 */
void lenv_def(lenv_t* e, lval_t* k, lval_t* v) {
    /* Iterate till e has no parent */
    while (e->par) { e = e->par; }
    /* Put value in e */
    lenv_put(e, k, v);
}


void lenv_copy2(lenv_t* dst, lenv_t* src) {
    lval_t s;

    for (size_t i = 0; i < src->count; i++) {
        s.sym = src->syms[i];
        lenv_put(dst, &s, src->vals[i]);
    }

    dst->par = src->par;
}

lenv_t* lenv_copy(lenv_t* e) {
    lenv_t* n_env = lenv_new();
    lenv_copy2(n_env, e);
    return n_env;
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
        case String: lval_print_str(v); break;
        case Func:
            if (v->func.builtin) {
                printf("<builtin>");
            }
            else {
                printf("(\\ ");
                lval_print(v->func.formals);
                putchar(' ');
                lval_print(v->func.body);
                printf(" )");
            }
            break;

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

void lval_print_str(lval_t* v) {
    /* make a copy */
    char* escapted = malloc(strlen(v->str) + 1);
    strcpy(escapted, v->str);

    /* pass it through the escape function */
    escapted = mpcf_escape(escapted);

    /* print */
    printf("\"%s\"", escapted);
    free(escapted);
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

Result parse_string(mpc_ast_t* t) {
    char* unescaped;
    lval_t* str;

    /* Remove final quote */
    t->contents[strlen(t->contents)-1] = '\0';
    /* Remove leading quote*/
    unescaped = malloc(strlen(t->contents+1)-1);
    strcpy(unescaped, t->contents+1);

    /* pass through unescaped function */
    unescaped = mpcf_unescape(unescaped);

    str = lval_str(unescaped);
    free(unescaped);

    return Ok(str);
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
    if (strstr(a->tag, "string")) return parse_string(a);

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
        /* ignore comments */
        if (strstr(a->children[i]->tag, "comment")) continue;

        res = ast_to_lval(a->children[i]);
        if (isErr(&res)) {
            if (strcmp(res.err, DONT_CARE) == 0) {
                continue;
            }
            else {
                /* Cleanup any alloc'd lvals on error */
                lval_del(lval);
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

    assert(i < v->count);

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
    lval_del(v);
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

    lval_del(y);
    return x;
}

bool lval_eq( lval_t* x, lval_t* y) {
    /* Different types are always unequal */
    if (x->type != y->type) return false;

    switch (x->type) {
    /* Compare numbers */
    case Number: return x->num == y->num;

    /* Compare strings */
    case Symbol: return strcmp(x->sym, y->sym) == 0;
    case String: return strcmp(x->str, y->str) == 0;

    /* If Func, compare formals and body */
    case Func:
        if (x->func.builtin || y->func.builtin) {
            return x->func.builtin == y->func.builtin;
        } else {
            return lval_eq(x->func.formals, y->func.formals)
                && lval_eq(x->func.body, y->func.body);
        }

    case QExpression:
    case SExpression:
        if (x->count != y->count ) return false;

        for (size_t i = 0; i < x->count; i++ ) {
            if (!lval_eq(x->cell[i], y->cell[i])) return false;
        }

        return true;
    }

    return false;
}


/**
 * @brief Performs operation on S-Expression and return result. The given
 *  lval_t is consumed.
 *
 * On error Err is returned.
 *
 * @param op '+', '-', '*', '/', etc.
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

            if (a->count == 1 && strcmp(op, "-") == 0) {
                /* unary negation */
                accum = -accum;
            }

            continue;
        }

        if (strcmp(op, "+") == 0) {
            accum += v->num;
        }
        else if (strcmp(op, "-") == 0) {
            /* normal subtraction */
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
    lval_del(a);

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
 * @brief Performs comparison on S-Expression and return result. The given
 *  lval_t is consumed.
 *
 * On error Err is returned.
 *
 * @param op
 * @param l
 * @param r
 * @return Result
 */
Result builtin_ord(lenv_t* e, lval_t* a, char* op) {
    UNUSED(e);
    Result res;
    bool accum;
    lval_t *l, *r;

    /* Validate two args and are Numbers */
    LASSERT_NUM("op", a, 2);
    LASSERT_TYPE("op", a, 0, Number);
    LASSERT_TYPE("op", a, 1, Number);

    l = a->cell[0];
    r = a->cell[1];

    /* Do ordering operation */
    if (strcmp(op, ">") == 0) {
        accum = l->num > r->num;
    }
    else if (strcmp(op, ">=") == 0) {
        accum = l->num >= r->num;
    }
    else if (strcmp(op, "<") == 0) {
        accum = l->num < r->num;
    }
    else if (strcmp(op, "<=") == 0) {
        accum = l->num <= r->num;
    }
    else {
        res = printErr("BadOp: %s", op);
        goto finish_op;
    }

    /* Return as a number. */
    res = Ok(lval_num(accum ? 1 : 0));

finish_op:
    /* consume lval */
    lval_del(a);

    return res;
} /* builtin_ord() */


Result builtin_gt(lenv_t* e, lval_t* a) {
  return builtin_ord(e, a, ">");
}

Result builtin_ge(lenv_t* e, lval_t* a) {
  return builtin_ord(e, a, ">=");
}

Result builtin_lt(lenv_t* e, lval_t* a) {
  return builtin_ord(e, a, "<");
}

Result builtin_le(lenv_t* e, lval_t* a) {
  return builtin_ord(e, a, "<=");
}


Result builtin_cmp(lenv_t* e, lval_t* a, char* op) {
    UNUSED(e);
    Result res;
    bool accum;
    lval_t *l, *r;

    /* Validate two args */
    LASSERT_NUM("op", a, 2);

    l = a->cell[0];
    r = a->cell[1];

    if (strcmp(op, "==") == 0) {
        accum = lval_eq(l, r);
    }
    else if (strcmp(op, "!=") == 0) {
        accum = !lval_eq(l, r);
    }
    else {
        res = printErr("BadOp: %s", op);
        goto finish_op;
    }

    /* Return as a number. */
    res = Ok(lval_num(accum ? 1 : 0));

finish_op:
    /* consume lval */
    lval_del(a);

    return res;
}

Result builtin_eq(lenv_t* e, lval_t* a) {
    return builtin_cmp(e, a, "==");
}

Result builtin_ne(lenv_t* e, lval_t* a) {
    return builtin_cmp(e, a, "!=");
}


Result builtin_logical(lenv_t* e, lval_t* a, char* op) {
        UNUSED(e);
    Result res;
    bool accum;
    lval_t *l, *r;

    /* Validate two args */
    LASSERT_NUM("op", a, 2);
    LASSERT_TYPE("op", a, 0, Number);
    LASSERT_TYPE("op", a, 1, Number);

    l = a->cell[0];
    r = a->cell[1];

    if (strcmp(op, "&&") == 0) {
        accum = l->num && r->num;
    }
    else if (strcmp(op, "||") == 0) {
        accum = l->num || r->num;
    }
    else {
        res = printErr("BadOp: %s", op);
        goto finish_op;
    }

    /* Return as a number. */
    res = Ok(lval_num(accum ? 1 : 0));

finish_op:
    /* consume lval */
    lval_del(a);

    return res;
}

Result builtin_and(lenv_t* e, lval_t* a) {
    return builtin_logical(e, a, "&&");
}

Result builtin_or(lenv_t* e, lval_t* a) {
    return builtin_logical(e, a, "||");
}

Result builtin_not(lenv_t* e, lval_t* a) {
    UNUSED(e);
    LASSERT_NUM("!", a, 1);
    LASSERT_TYPE("!", a, 0, Number);

    /* Return inverted value */
    return Ok(lval_num(a->cell[0]->num ? 0 : 1));
}

Result builtin_if(lenv_t* e, lval_t* a) {
    LASSERT_NUM("if", a, 3);
    LASSERT_TYPE("if", a, 0, Number);
    LASSERT_TYPE("if", a, 1, QExpression);
    LASSERT_TYPE("if", a, 2, QExpression);

    Result x;

    /* Mark both expressions as evaluatable */
    a->cell[1]->type = SExpression;
    a->cell[2]->type = SExpression;

    if (a->cell[0]->num) {
        /* TRUE */
        x = lval_eval(e, lval_pop(a, 1));
    } else {
        /* FALSE */
        x = lval_eval(e, lval_pop(a, 2));
    }

    lval_del(a);
    return x;
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

    LASSERT_NUM("head", a, 1);
    LASSERT_TYPE("head", a, 0, QExpression);
    LASSERT(a, 0 != a->cell[0]->count,
        "List passed to 'head' is empty!");

    v = lval_take(a, 0);
    while (v->count > 1) { lval_del(lval_pop(v, 1)); }

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

    LASSERT_NUM("tail", a, 1);
    LASSERT_TYPE("tail", a, 0, QExpression);
    LASSERT(a, 0 != a->cell[0]->count,
        "Func 'tail' passed empty list!");

    v = lval_take(a, 0);
    lval_del(lval_pop(v, 0));

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

    LASSERT_NUM("eval", a, 1);
    LASSERT_TYPE("eval", a, 0, QExpression);

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
        LASSERT_TYPE("join", a, i, QExpression);
    }

    /* grab the 1st argument */
    lval_t* x = lval_pop(a, 0);

    /* append every other argument's contents to the first */
    while (a->count) {
        x = lval_join(x, lval_pop(a, 0));
    }

    lval_del(a);
    return Ok(x);
}


Result builtin_var(lenv_t* e, lval_t* a, char* func) {
    LASSERT_TYPE("func", a, 0, QExpression);

    /* 1st arg is symbol list, remaining are values */
    lval_t* syms = a->cell[0];

    /* Validate all list elements are symbols */
    for (size_t i = 0; i < syms->count; i++) {
        LASSERT(a, syms->cell[i]->type == Symbol,
            "Function '%s' cannot define non-symbol. "
            "Act: %s, Exp: %s",
            str_type(syms->cell[i]->type), str_type(Symbol),
            func);
    }

    /* Number of symbols must eq number of values */
    LASSERT(a, syms->count == a->count-1,
        "Function '%s', the number of symbols (%d) does not match the number of values (%d).",
        func,
        syms->count, a->count-1);

    /* Assign k:v pairs to the apropriate environment */
    for (size_t i = 0; i < syms->count; i++) {
        if (0 == strcmp(func, "def")) {
            lenv_def(e, syms->cell[i], a->cell[1+i]);
        }

        if (0 == strcmp(func, "=")) {
            lenv_put(e, syms->cell[i], a->cell[1+i]);
        }
    }

    lval_del(a);
    return Ok(lval_sexpr());
}

Result builtin_def(lenv_t* e, lval_t* a) {
    return builtin_var(e, a, "def");
}

Result builtin_put(lenv_t* e, lval_t* a) {
    return builtin_var(e, a, "=");
}

Result builtin_lambda(lenv_t* e, lval_t* a) {
    UNUSED(e);

    /* Two args, each are Q-expressions */
    LASSERT_NUM("\\", a, 2);
    LASSERT_TYPE("\\", a, 0, QExpression);
    LASSERT_TYPE("\\", a, 1, QExpression);

    /* Check first arg contains only symbols */
    for (size_t i = 0; i < a->cell[0]->count; i++) {
        LASSERT(a, Symbol == a->cell[0]->cell[i]->type,
            "Cannot define non-symbol. Act: %s, Exp: %s",
            str_type(a->cell[i]->cell[i]->type), str_type(Symbol));
    }

    lval_t* formals = lval_pop(a, 0);
    lval_t* body = lval_pop(a, 0);

    /* consume lval */
    lval_del(a);

    return Ok(lval_lamda(formals, body));
}

Result builtin_load(lenv_t* e, lval_t* a) {
    LASSERT_NUM("load", a, 1);
    LASSERT_TYPE("load", a, 0, String);

    Result res;
    mpc_result_t r;
    lval_t* expr;

    /* Parse file given by string */
    if (mpc_parse_contents(a->cell[0]->str, Lispy, &r)) {
        /* Success: Print the AST */
        // mpc_ast_print(r.output);

        /* Parse AST */
        res = ast_to_lval(r.output);
        mpc_ast_delete(r.output);

        if (isErr(&res)) {
            err_print(res.err);
        } else {
            // lval_println(res.lval);

            expr = res.lval;

            /* Evaluate expression */
            while (expr->count) {

                res = lval_eval(e, lval_pop(expr, 0));
                if (isErr(&res)) {
                    err_print(res.err);
                } else {
                    lval_del(res.lval);
                }
            }

            lval_del(expr);
        }

        lval_del(a);

        return Ok(lval_sexpr());
    } else {
        /* failure: Print the error */
        char* err_msg;
        err_msg = mpc_err_string(r.error);
        mpc_err_delete(r.error);
        res = printErr("Could not load Library %s", err_msg);
        free(err_msg);
        lval_del(a);
        return res;
    }
}

Result builtin_print(lenv_t* e, lval_t* a) {
    UNUSED(e);
    /* Print each argument followed by a space */
    for (size_t i = 0; i < a->count; i++) {
        lval_print(a->cell[i]);
        putchar(' ');
    }

    /* Print a newline and delete arguments */
    putchar('\n');
    lval_del(a);

    return Ok(lval_sexpr());
}

Result builtin_error(lenv_t* e, lval_t* a) {
    UNUSED(e);
    LASSERT_NUM("error", a, 1);
    LASSERT_TYPE("error", a, 0, String);

    Result err;
    err = Err(a->cell[0]->str);

    lval_del(a);
    return err;
}


void lenv_add_builtin(lenv_t* e, char* name, lbuiltin func) {
    lval_t* k = lval_sym(name);
    lval_t* v = lval_func(func);
    lenv_put(e, k, v);
    lval_del(k);
    lval_del(v);
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

    /* Comparison Functions */
    lenv_add_builtin(e, "if", builtin_if);
    lenv_add_builtin(e, "==", builtin_eq);
    lenv_add_builtin(e, "!=", builtin_ne);
    lenv_add_builtin(e, ">",  builtin_gt);
    lenv_add_builtin(e, ">=", builtin_ge);
    lenv_add_builtin(e, "<",  builtin_lt);
    lenv_add_builtin(e, "<=", builtin_le);
    lenv_add_builtin(e, "&&", builtin_and);
    lenv_add_builtin(e, "||", builtin_or);
    lenv_add_builtin(e, "!", builtin_not);

    /* Variable Functions */
    lenv_add_builtin(e, "def", builtin_def);
    lenv_add_builtin(e, "=",   builtin_put);
    lenv_add_builtin(e, "\\",  builtin_lambda);

    lenv_add_builtin(e, "load",  builtin_load);
    lenv_add_builtin(e, "error",  builtin_error);
    lenv_add_builtin(e, "print",  builtin_print);

}

Result lval_call(lenv_t* e, lval_t* f, lval_t* a) {

    /* Call builtin functions directly */
    if (f->func.builtin) {
        return f->func.builtin(e, a);
    }

    /* Process Arguments */
    while (a->count) {
        /* Pop arg name and arg value */
        lval_t* sym = lval_pop(f->func.formals, 0);
        lval_t* val = lval_pop(a, 0);

        /* Check for variable args */
        if ('&' == *sym->sym) {
            /* Check only one arg follows the & */
            if (1 != f->func.formals->count) {
                lval_del(a);
                return printErr("Incorrect number of args appear after '&'. Exp: 1, Act: %d",f->func.formals->count);
            }

            /* bind rest of args to varg list variable */
            lval_t* nsym = lval_pop(f->func.formals, 0);
            lenv_put(f->func.env, nsym, builtin_list(e, a).lval);
            lval_del(sym); lval_del(nsym);
            break;
        }

        /* Place in local environment */
        lenv_put(f->func.env, sym, val);

        /* cleanup */
        lval_del(sym);
        lval_del(val);
    }

    /* Cleanup arg list */
    lval_del(a);

    /* If '&' remains in formal list, bind to empty list */
    if ( (0 < f->func.formals->count)
      && (0 == strcmp(f->func.formals->cell[0]->sym, "&")) ) {

        /* Check to ensure that & is not passed invalidly. */
        if (f->func.formals->count != 2) {
            return Err("Function format invalid. "
            "Symbol '&' not followed by single symbol.");
        }

        /* Pop and delete '&' symbol */
        lval_del(lval_pop(f->func.formals, 0));

        /* Pop next symbol and create empty list */
        lval_t* sym = lval_pop(f->func.formals, 0);
        lval_t* val = lval_qexpr();

        /* Bind to environment and delete */
        lenv_put(f->func.env, sym, val);
        lval_del(sym); lval_del(val);
    }

    /* Evaluate if all formals were bound */
    if (0 == f->func.formals->count) {
        /* Set Parent environment */
        f->func.env->par = e;

        /* Evaluate body */
        return builtin_eval(
                    f->func.env,
                    lval_add(lval_sexpr(), lval_copy(f->func.body)));
    }
    /* Otherwise, return partialy evaluated function */
    else {
        return Ok(lval_copy(f));
    }
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
        lval_del(v);
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
        lval_del(f);
        lval_del(v);
        return Err("First element is not a function");
    }

    /* Call function */
    res = lval_call(e, f, v);

    lval_del(f);
    return res;
} /* lval_eval_sexpr() */


int main(int argc, char** argv) {
    Result res;

    /* Setup grammer for Polish Notation */
    mpc_parser_t* Number;
    mpc_parser_t* Symbol;
    mpc_parser_t* Strings;
    mpc_parser_t* Comment;
    mpc_parser_t* Sexpr;
    mpc_parser_t* Qexpr;
    mpc_parser_t* Expr;

    Number = mpc_new("number");
    Symbol = mpc_new("symbol");
    Strings = mpc_new("string");
    Comment = mpc_new("comment");
    Sexpr  = mpc_new("sexpr");
    Qexpr  = mpc_new("qexpr");
    Expr   = mpc_new("expr");
    Lispy  = mpc_new("lispy");

    mpca_lang(MPCA_LANG_DEFAULT,
        "                                               \
        number : /-?[0-9]+/ ;                           \
        symbol : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&|]+/ ;    \
        string  : /\"(\\\\.|[^\"])*\"/ ;                \
        comment : /;[^\\r\\n]*/ ;                       \
        sexpr  : '(' <expr>* ')' ;                      \
        qexpr  : '{' <expr>* '}' ;                      \
        expr   : <number> | <symbol> | <string> |       \
                 <comment> | <sexpr> | <qexpr> ;        \
        lispy  : /^/ <expr>* /$/ ;                      \
        ",
    Number, Symbol, Strings, Comment, Sexpr, Qexpr, Expr, Lispy);

    /* Print Version and Exit Information */
    puts("Lispy Version 0.0.0.0.1");
    puts("Press Ctrl+c to Exit\n");

    /* Setup environment */
    lenv_t* e = lenv_new();
    lenv_add_builtins(e);

    /* Load any files given on command line */
    if (argc >= 2) {
        /* Loop over given filenames */
        for (int i = 0; i < argc; i++) {
            /* Setup arg list for "load" */
            lval_t* args = lval_add(lval_sexpr(), lval_str(argv[i]));

            /* call "load" */
            Result x = builtin_load(e, args);

            if (isErr(&x)) err_print(x.err);
            else lval_del(x.lval);
        }
    }

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
                    lval_del(res.lval);
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
    mpc_cleanup(6, Number, Symbol, Strings, Comment, Sexpr, Qexpr, Expr, Lispy);

    return 0;
}