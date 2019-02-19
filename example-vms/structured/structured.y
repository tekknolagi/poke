/* Jittery structured language example: Bison parser.

   Copyright (C) 2016, 2017, 2019 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jitter structured-language example, distributed
   along with Jitter under the same license.

   Jitter is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Jitter is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jitter.  If not, see <http://www.gnu.org/licenses/>. */


/* This code does not go to the generated header. */
%{
#include <config.h>

#include <stdio.h>
#include <ctype.h>
#include <jitter/jitter-malloc.h>
#include <jitter/jitter-fatal.h>
#include <jitter/jitter-parse-int.h>
#include <jitter/jitter-string.h>
  //#include <gl_xlist.h>
  //#include <gl_array_list.h>

#include "structured-syntax.h"
#include "structured-parser.h"
#include "structured-scanner.h"

/* This is currently a fatal error.  I could longjmp away instead. */
static void
structured_error (YYLTYPE *locp, struct structured_program *p,
                  yyscan_t scanner, char *message)
  __attribute__ ((noreturn));

#define STRUCTURED_PARSE_ERROR(message)                            \
  do                                                               \
    {                                                              \
      structured_error (structured_get_lloc (structured_scanner),  \
                        p, structured_scanner, message);           \
    }                                                              \
  while (false)

/* What would be yytext in a non-reentrant scanner. */
#define STRUCTURED_TEXT \
  (structured_get_text (structured_scanner))

 /* What would be yylineno in a non-reentrant scanner. */
#define STRUCTURED_LINENO \
  (structured_get_lineno (structured_scanner))

/* A copy of what would be yytext in a non-reentrant scanner. */
#define STRUCTURED_TEXT_COPY \
  (jitter_clone_string (STRUCTURED_TEXT))

/* Return a pointer to a fresh malloc-allocated expression of the given case.
   No field is initialized but case_. */
static struct structured_expression*
structured_make_expression (enum structured_expression_case case_)
{
  struct structured_expression *res
    = jitter_xmalloc (sizeof (struct structured_expression));
  res->case_ = case_;

  return res;
}

/* Return a pointer to a fresh malloc-allocated expression of the primitive
   case, with the given binary primitive and operands.  Every field is
   initalized. */
static struct structured_expression*
structured_make_binary (enum structured_primitive primitive,
                        struct structured_expression *operand_0,
                        struct structured_expression *operand_1)
{
  struct structured_expression *res
    = structured_make_expression (structured_expression_case_primitive);
  res->primitive = primitive;
  res->primitive_operand_0 = operand_0;
  res->primitive_operand_1 = operand_1;
  return res;
}

/* Return a pointer to a fresh malloc-allocated expression of the primitive
   case, with the given nullary primitive.  Every field is initalized. */
static struct structured_expression*
structured_make_nullary (enum structured_primitive primitive)
{
  return structured_make_binary (primitive, NULL, NULL);
}

/* Return a pointer to a fresh malloc-allocated expression of the primitive
   case, with the given unary primitive and operand.  Every field is
   initalized. */
static struct structured_expression*
structured_make_unary (enum structured_primitive primitive,
                       struct structured_expression *operand_0)
{
  return structured_make_binary (primitive, operand_0, NULL);
}

/* Return a pointer to a fresh malloc-allocated statement of the given case.
   No field is initialized but case_. */
static struct structured_statement*
structured_make_statement (enum structured_statement_case case_)
{
  struct structured_statement *res
    = jitter_xmalloc (sizeof (struct structured_statement));
  res->case_ = case_;

  return res;
}

/* If the pointed expressions is non-NULL return a pointer to a fresh
   malloc-allocated statement containing a sequence setting the given variable
   to the pointed expression, and then the pointed statement.  If the expression
   pointer is NULL just return the pointed body without allocating anything
   more. */
static struct structured_statement*
structured_with_optional_initialization (structured_variable v,
                                         struct structured_expression *e,
                                         struct structured_statement *body)
{
  if (e == NULL)
    return body;
  else
    {
      struct structured_statement *sequence
        = structured_make_statement (structured_statement_case_sequence);
      struct structured_statement *assignment
        = structured_make_statement (structured_statement_case_assignment);
      assignment->assignment_variable = v;
      assignment->assignment_expression = e;
      sequence->sequence_statement_0 = assignment;
      sequence->sequence_statement_1 = body;
      return sequence;
    }
}

%}

/* We need a recent enough version of GNU Bison. */
%require "2.3b" /* This is the first version supporting %define api.pure . */

/* Use a prefix different from the default "yy" for the API. */
%name-prefix "structured_" /* FIXME: use a different prefix and find a way
                              of linking together several Bison/Flex
                              parsers and scanners (even if I don't need that
                              for the generator); I think I currently have
                              a problem with the Flex part. */

// FIXME: the Bison documentation says that this is obsolete.  I should use
// %define api.prefix {structured_}
// instead.  That will let me use more than one Bison parser in the
// same executable.

/* Generate a header file. */
%defines

/* This is a reentrant parser. */
/*%define api.pure full*/ /* FIXME: I'd need to %require "3.0" for this.  Do I
                             care about the difference?  Probably not. */
%define api.pure

/* We need to receive location information from the scanner, Bison-style. */
%locations

/* The parser and scanner functions both have additional parameters. */
%lex-param { structured_scan_t structured_scanner }
%parse-param { struct structured_program *p }
%parse-param { void* structured_scanner }

/* We don't need a %initial-action block, because the parser receives an already
   initialized data structure; see the definition of structured_parse_file_star . */

/* This goes to the parser header file. */
%code requires {
/* Simplified error-reporting facilities calling structured_error, suitable to be
   called from the scanner and the parser without the complicated and
   irrelevant parameters needed by structured_error . */
void
structured_scan_error (void *structured_scanner) __attribute__ ((noreturn));

struct structured_program *
structured_parse_file_star (FILE *input_file);

struct structured_program *
structured_parse_file (const char *input_file_name);
} /* end of %code requires */

%union
{
  jitter_int literal;
  structured_variable variable;
  struct structured_expression *expression;
  struct structured_statement *statement;
}

%token BEGIN_ END
%token SKIP
%token VAR
%token PRINT
%token INPUT
%token SET_TO
%token SEMICOLON
%token COMMA
%token IF THEN ELSE ELIF
%token WHILE DO
%token REPEAT UNTIL
%token OPEN_PAREN CLOSE_PAREN
%token VARIABLE
       /*BINARY_LITERAL OCTAL_LITERAL*/ DECIMAL_LITERAL /*HEXADECIMAL_LITERAL*/
       TRUE FALSE
%left PLUS MINUS
%left TIMES
%left DIVIDED REMAINDER
%left EQUAL DIFFERENT LESS LESS_OR_EQUAL GREATER GREATER_OR_EQUAL
%left LOGICAL_OR
%left LOGICAL_AND
%left LOGICAL_NOT
%precedence UNARY_MINUS

%type <literal> literal;
%type <variable> variable;
%type <expression> expression;
%type <statement> statement;
%type <statement> statements;
%type <statement> one_or_more_statements;
%type <statement> block;
%type <statement> block_rest;
%type <statement> if_statement;
%type <statement> if_statement_rest;
%type <expression> optional_initialization;
%type <expression> if_expression;
%type <expression> if_expression_rest;

%%

program:
  statements
  { p->main_statement = $1; }
  ;

statement:
  optional_skip SEMICOLON
  { $$ = structured_make_statement (structured_statement_case_skip); }
| variable SET_TO expression SEMICOLON
  { $$ = structured_make_statement (structured_statement_case_assignment);
    $$->assignment_variable = $1;
    $$->assignment_expression = $3; }
| PRINT expression SEMICOLON
  { $$ = structured_make_statement (structured_statement_case_print);
    $$->print_expression = $2; }
| begin statements end
  { $$ = $2; }
| if_statement
  { $$ = $1; }
| WHILE expression DO statements end
  { /* Parse "while A do B end" as "if A then repeat B until not A else
       skip". */
    struct structured_statement *r
      = structured_make_statement (structured_statement_case_repeat_until);
    r->repeat_until_body = $4;
    /* FIXME: clone $2 into a separate heap object, if I want to be able to free
       ASTs. */
    r->repeat_until_guard
      = structured_make_unary (structured_primitive_logical_not, $2);
    $$ = structured_make_statement (structured_statement_case_if_then_else);
    $$->if_then_else_condition = $2;
    $$->if_then_else_then_branch = r;
    $$->if_then_else_else_branch
      = structured_make_statement (structured_statement_case_skip); }
| REPEAT statements UNTIL expression SEMICOLON
  { $$ = structured_make_statement (structured_statement_case_repeat_until);
    $$->repeat_until_body = $2;
    $$->repeat_until_guard = $4; }
  ;

if_statement:
  IF expression THEN statements if_statement_rest
  { $$ = structured_make_statement (structured_statement_case_if_then_else);
    $$->if_then_else_condition = $2;
    $$->if_then_else_then_branch = $4;
    $$->if_then_else_else_branch = $5; }
;

if_statement_rest:
  end
  { /* Parse "if A then B end" as "if A then B else skip end". */
    $$ = structured_make_statement (structured_statement_case_skip); }
| ELIF expression THEN statements if_statement_rest
  { $$ = structured_make_statement (structured_statement_case_if_then_else);
    $$->if_then_else_condition = $2;
    $$->if_then_else_then_branch = $4;
    $$->if_then_else_else_branch = $5; }
| ELSE statements end
  { $$ = $2; }
;

statements:
  /* nothing */
  { $$ = structured_make_statement (structured_statement_case_skip); }
| one_or_more_statements
  { $$ = $1; }
  ;

one_or_more_statements:
  statement
  { $$ = $1; }
| statement one_or_more_statements
  { $$ = structured_make_statement (structured_statement_case_sequence);
    $$->sequence_statement_0 = $1;
    $$->sequence_statement_1 = $2; }
| block
  { $$ = $1; }
  ;

block:
  VAR variable optional_initialization block_rest
  { $$ = structured_make_statement (structured_statement_case_block);
    $$->block_variable = $2;
    $$->block_body = structured_with_optional_initialization ($2, $3, $4); }
  ;

block_rest:
  SEMICOLON one_or_more_statements
  { $$ = $2; }
| COMMA block
  { $$ = $2; }
  ;

optional_initialization :
  /* nothing*/
  { $$ = NULL; }
| EQUAL expression
  { $$ = $2; }
  ;

expression:
  literal
  { $$ = structured_make_expression (structured_expression_case_literal);
    $$->literal = $1; }
| variable
  { $$ = structured_make_expression (structured_expression_case_variable);
    $$->variable = $1; }
| OPEN_PAREN expression CLOSE_PAREN
  { $$ = $2; }
| if_expression
  { $$ = $1; }
| expression PLUS expression
  { $$ = structured_make_binary (structured_primitive_plus, $1, $3); }
| expression MINUS expression
  { $$ = structured_make_binary (structured_primitive_minus, $1, $3); }
| MINUS expression %prec UNARY_MINUS
  { $$ = structured_make_unary (structured_primitive_unary_minus, $2); }
| expression TIMES expression
  { $$ = structured_make_binary (structured_primitive_times, $1, $3); }
| expression DIVIDED expression
  { $$ = structured_make_binary (structured_primitive_divided, $1, $3); }
| expression REMAINDER expression
  { $$ = structured_make_binary (structured_primitive_remainder, $1, $3); }
| expression EQUAL expression
  { $$ = structured_make_binary (structured_primitive_equal, $1, $3); }
| expression DIFFERENT expression
  { $$ = structured_make_binary (structured_primitive_different, $1, $3); }
| expression LESS expression
  { $$ = structured_make_binary (structured_primitive_less, $1, $3); }
| expression LESS_OR_EQUAL expression
  { $$ = structured_make_binary (structured_primitive_less_or_equal, $1, $3); }
| expression GREATER expression
  { $$ = structured_make_binary (structured_primitive_greater, $1, $3); }
| expression GREATER_OR_EQUAL expression
  { $$ = structured_make_binary (structured_primitive_greater_or_equal, $1, $3); }
| expression LOGICAL_AND expression
  { /* Parse "A and B" as "if A then B else false end". */
    $$ = structured_make_expression (structured_expression_case_if_then_else);
    $$->if_then_else_condition = $1;
    $$->if_then_else_then_branch = $3;
    $$->if_then_else_else_branch
      = structured_make_expression (structured_expression_case_literal);
    $$->if_then_else_else_branch->literal = 0; }
| expression LOGICAL_OR expression
  { /* Parse "A or B" as "if A then true else B end". */
    $$ = structured_make_expression (structured_expression_case_if_then_else);
    $$->if_then_else_condition = $1;
    $$->if_then_else_then_branch
      = structured_make_expression (structured_expression_case_literal);
    $$->if_then_else_then_branch->literal = 1;
    $$->if_then_else_else_branch = $3; }
| LOGICAL_NOT expression
  { $$ = structured_make_unary (structured_primitive_logical_not, $2); }
| INPUT
  { $$ = structured_make_nullary (structured_primitive_input); }
  ;

if_expression:
  IF expression THEN expression if_expression_rest
  { $$ = structured_make_expression (structured_expression_case_if_then_else);
    $$->if_then_else_condition = $2;
    $$->if_then_else_then_branch = $4;
    $$->if_then_else_else_branch = $5; }
;

if_expression_rest:
  /* For expressions there is no if..then..end without else; however elif
     clauses are permitted. */
  ELIF expression THEN expression if_expression_rest
  { $$ = structured_make_expression (structured_expression_case_if_then_else);
    $$->if_then_else_condition = $2;
    $$->if_then_else_then_branch = $4;
    $$->if_then_else_else_branch = $5; }
| ELSE expression END
  { $$ = $2; }
;

literal:
  DECIMAL_LITERAL
  { $$ = jitter_string_to_long_long_unsafe (STRUCTURED_TEXT); }
| TRUE
  { $$ = 1; }
| FALSE
  { $$ = 0; }
  ;

variable:
  VARIABLE
  { $$ = STRUCTURED_TEXT_COPY; }
  ;

optional_skip:
  /* nothing */
| SKIP
  ;

/* No need for optional semicolons after BEGIN_: any semicolons after it will be
   parsed as skip statements. */
begin:
  BEGIN_
  ;

/* No need for optional semicolons after END: any semicolons after it will be
   parsed as skip statements, since any context where END may occur accepts a
   statement sequence, and not just a statement. */
end:
  END
  ;


%%

void
structured_error (YYLTYPE *locp, struct structured_program *p, yyscan_t structured_scanner,
                 char *message)
{
  printf ("%s:%i: %s near \"%s\".\n",
          (p != NULL) ? p->source_file_name : "<INPUT>",
          structured_get_lineno (structured_scanner), message, STRUCTURED_TEXT);
  exit (EXIT_FAILURE);
}

void
structured_scan_error (void *structured_scanner)
{
  struct structured_program *p = NULL; /* A little hack to have p in scope. */
  STRUCTURED_PARSE_ERROR("scan error");
}

static struct structured_program *
structured_parse_file_star_with_name (FILE *input_file, const char *file_name)
{
  yyscan_t scanner;
  structured_lex_init (&scanner);
  structured_set_in (input_file, scanner);

  struct structured_program *res
    = jitter_xmalloc (sizeof (struct structured_program));
  res->source_file_name = jitter_clone_string (file_name);
  /* FIXME: if I ever make parsing errors non-fatal, call structured_lex_destroy before
     returning, and finalize the program -- which might be incomplete! */
  if (structured_parse (res, scanner))
    structured_error (structured_get_lloc (scanner), res, scanner, "parse error");
  structured_set_in (NULL, scanner);
  structured_lex_destroy (scanner);

  return res;
}

struct structured_program *
structured_parse_file_star (FILE *input_file)
{
  return structured_parse_file_star_with_name (input_file, "<stdin>");
}

struct structured_program *
structured_parse_file (const char *input_file_name)
{
  FILE *f;
  if ((f = fopen (input_file_name, "r")) == NULL)
    jitter_fatal ("failed opening file %s", input_file_name);

  /* FIXME: if I ever make parse errors non-fatal, I'll need to close the file
     before returning. */
  struct structured_program *res
    = structured_parse_file_star_with_name (f, input_file_name);
  fclose (f);
  return res;
}
