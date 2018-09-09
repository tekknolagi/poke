/* pkl-gen.c - Code generation phase for the poke compiler.  */

/* Copyright (C) 2018 Jose E. Marchesi */

/* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>
#include <stdio.h>
#include <assert.h>
#include <jitter/jitter.h>

#include "pkl.h"
#include "pkl-gen.h"
#include "pkl-ast.h"
#include "pkl-pass.h"
#include "pkl-asm.h"
#include "pvm.h"

/* The following macros are used in the rules below, to reduce
   verbosity.  */

#define PKL_GEN_PAYLOAD ((pkl_gen_payload) PKL_PASS_PAYLOAD)

#define PKL_GEN_ASM                                     \
  (PKL_GEN_PAYLOAD->pasm[PKL_GEN_PAYLOAD->cur_pasm])

#define PKL_GEN_PUSH_ASM(new_pasm)                                    \
  do                                                                  \
    {                                                                 \
      assert (PKL_GEN_PAYLOAD->cur_pasm < PKL_GEN_MAX_PASM);          \
      PKL_GEN_PAYLOAD->pasm[++(PKL_GEN_PAYLOAD->cur_pasm)] = (new_pasm); \
    }                                                                 \
  while (0)

#define PKL_GEN_POP_ASM                         \
  do                                            \
    {                                           \
      assert (PKL_GEN_PAYLOAD->cur_pasm > 0);   \
      PKL_GEN_PAYLOAD->cur_pasm -= 1;           \
    }                                           \
  while (0)

/*
 * PROGRAM
 * | PROGRAM_ELEM
 * | ...
 *
 * This function initializes the payload and also generates the
 * standard prologue.
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_program)
{
  PKL_GEN_ASM = pkl_asm_new (PKL_PASS_AST, 1 /* guard_stack */);
}
PKL_PHASE_END_HANDLER

/*
 * | PROGRAM_ELEM
 * | ...
 * PROGRAM
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_program)
{
  PKL_GEN_PAYLOAD->program = pkl_asm_finish (PKL_GEN_ASM);
}
PKL_PHASE_END_HANDLER

/*
 * DECL
 * | INITIAL
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_decl)
{
  pkl_ast_node decl = PKL_PASS_NODE;

  if (PKL_AST_DECL_KIND (decl) == PKL_AST_DECL_KIND_FUNC)
    {
      /* INITIAL is a PKL_AST_FUNC, that will compile into a program
         containing the function code.  Push a new assembler to the
         stack of assemblers in the payload and use it to process
         INITIAL.  */

      PKL_GEN_PUSH_ASM (pkl_asm_new (PKL_PASS_AST, 0 /* guard_stack */));
    }
}
PKL_PHASE_END_HANDLER

/*
 * | INITIAL
 * DECL
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_decl)
{
  pkl_ast_node decl = PKL_PASS_NODE;

  switch (PKL_AST_DECL_KIND (decl))
    {
    case PKL_AST_DECL_KIND_VAR:
    case PKL_AST_DECL_KIND_TYPE:
      /* Nothing to do.  */
      break;
    case PKL_AST_DECL_KIND_FUNC:
      {
        /* At this point the code for the function specification
           INITIAL has been assembled in the current macroassembler.
           Finalize the program and put it in a PVM closure, along
           with the current environment.  */

        pvm_program program = pkl_asm_finish (PKL_GEN_ASM);
        pvm_val closure;

        PKL_GEN_POP_ASM;
        pvm_specialize_program (program);
        closure = pvm_make_cls (program);

        /*XXX*/
        /*        pvm_print_program (stdout, program); */

        pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSH, closure);
        pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PEC);
        break;
      }
    default:
      assert (0);
      break;
    }

  /* If we are not at the top-level, push a new frame in the run-time
     environment.  */
  if (PKL_PASS_PARENT
      && PKL_AST_CODE (PKL_PASS_PARENT) != PKL_AST_PROGRAM)
    pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSHF);

  /* Finally register the declared entity, unless it is a type.  Types
     are not variables.  */
  if (PKL_AST_DECL_KIND (decl) != PKL_AST_DECL_KIND_TYPE)
    pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_REGVAR);
}
PKL_PHASE_END_HANDLER

/*
 * VAR
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_var)
{
  pkl_ast_node var = PKL_PASS_NODE;

  if (PKL_PASS_PARENT
      && PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_ASS_STMT)
    /* This is a l-value in an assignment.  Generate nothing, as this
       node is only used as a recipient for the lexical address of the
       variable.  */
    ;
  else
    pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSHVAR,
                  PKL_AST_VAR_BACK (var), PKL_AST_VAR_OVER (var));
}
PKL_PHASE_END_HANDLER

/*
 * NULL_STMT
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_null_stmt)
{
  /* Null is nothing, nada.  */
}
PKL_PHASE_END_HANDLER

/*
 * COMP_STMT
 * | (STMT | DECL)
 * | ...
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_comp_stmt)
{
  pkl_ast_node comp_stmt = PKL_PASS_NODE;

  if (PKL_AST_COMP_STMT_BUILTIN (comp_stmt) == PKL_AST_BUILTIN_NONE)
    {  
      /* Push a frame into the environment.  */
      pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSHF);
    }
}
PKL_PHASE_END_HANDLER

/*
 * | (STMT | DECL)
 * | ...
 * COMP_STMT
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_comp_stmt)
{
  pkl_ast_node comp_stmt = PKL_PASS_NODE;
  pkl_ast_node comp_stmt_stmts
    = PKL_AST_COMP_STMT_STMTS (comp_stmt);
  int comp_stmt_builtin
    = PKL_AST_COMP_STMT_BUILTIN (comp_stmt);

  if (comp_stmt_builtin != PKL_AST_BUILTIN_NONE)
    {
      switch (comp_stmt_builtin)
        {
        case PKL_AST_BUILTIN_PRINT:
          {
            /* defun print = (string s) __builtin_print __ */
            pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSHVAR, 0, 0);
            pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PRINT);
            break;
          }
        default:
            assert (0);
        }
    }
  else
    {
      pkl_ast_node stmt_decl;
      int num_frames = 0;

      /* Pop the frames created by the declarations contained in the
         compound statement from the enviroment.  */
      for (stmt_decl = comp_stmt_stmts;
           stmt_decl;
           stmt_decl = PKL_AST_CHAIN (stmt_decl))
        {
          if (PKL_AST_CODE (stmt_decl) == PKL_AST_DECL)
            num_frames++;
        }
      
      /* Note this includes the compound statement's frame.  */
      pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_POPF, num_frames + 1);
    }
}
PKL_PHASE_END_HANDLER

/*
 * | LVALUE
 * | EXP
 * ASS_STMT
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_ass_stmt)
{
  pkl_ast_node ass_stmt = PKL_PASS_NODE;
  pkl_ast_node lvalue = PKL_AST_ASS_STMT_LVALUE (ass_stmt);

  /* At this point the r-value, generated from executing EXP, is in
     the stack.  Note that `gen' didn't generate anything for LVALUE,
     as it is only used as a place-holder for the lexical address of
     the variable.  */

  assert (PKL_AST_CODE (lvalue) = PKL_AST_VAR);
  
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_POPVAR,
                PKL_AST_VAR_BACK (lvalue), PKL_AST_VAR_OVER (lvalue));
}
PKL_PHASE_END_HANDLER

/*
 * IF_STMT
 * | EXP
 * | THEN_STMT
 * | [ELSE_STMT]
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_if_stmt)
{
  pkl_ast_node if_stmt = PKL_PASS_NODE;
  pkl_ast_node if_exp = PKL_AST_IF_STMT_EXP (if_stmt);
  pkl_ast_node if_then_stmt = PKL_AST_IF_STMT_THEN_STMT (if_stmt);
  pkl_ast_node if_else_stmt = PKL_AST_IF_STMT_ELSE_STMT (if_stmt);

  pkl_asm_if (PKL_GEN_ASM, if_exp);
  {
    PKL_PASS_SUBPASS (if_exp);
  }
  pkl_asm_then (PKL_GEN_ASM);
  {
    PKL_PASS_SUBPASS (if_then_stmt);
  }
  pkl_asm_else (PKL_GEN_ASM);
  {
    if (if_else_stmt)
      PKL_PASS_SUBPASS (if_else_stmt);
  }
  pkl_asm_endif (PKL_GEN_ASM);

  PKL_PASS_BREAK;
}
PKL_PHASE_END_HANDLER

/*
 * LOOP_STMT
 * | CONDITION
 * | BODY
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_loop_stmt)
{
  pkl_ast_node loop_stmt = PKL_PASS_NODE;
  pkl_ast_node loop_stmt_condition
    = PKL_AST_LOOP_STMT_CONDITION (loop_stmt);
  pkl_ast_node loop_stmt_body
    = PKL_AST_LOOP_STMT_BODY (loop_stmt);

  pkl_asm_while (PKL_GEN_ASM);
  {
    PKL_PASS_SUBPASS (loop_stmt_condition);
  }
  pkl_asm_loop (PKL_GEN_ASM);
  {
    PKL_PASS_SUBPASS (loop_stmt_body);
  }
  pkl_asm_endloop (PKL_GEN_ASM);

  PKL_PASS_BREAK;
}
PKL_PHASE_END_HANDLER

/*
 * | EXP
 * RETURN
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_return_stmt)
{
  /* Return from the function: pop N frames and generate a return
     instruction.  Note the popf below includes the function's
     frame.  */

  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_POPF,
                PKL_AST_RETURN_STMT_NFRAMES (PKL_PASS_NODE) + 1);
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_RETURN);
}
PKL_PHASE_END_HANDLER

/*
 * | EXP
 * EXP_STMT
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_exp_stmt)
{
  /* Pop the expression from the stack.  */
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_POP);
}
PKL_PHASE_END_HANDLER

/*
 * | EXP
 * FUNCALL_ARG
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_funcall_arg)
{
  /* Do nothing, the argument is alread pushed in the stack.  */
}
PKL_PHASE_END_HANDLER

/*
 * | [ARG]...
 * | FUNCTION
 * FUNCALL
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_funcall)
{
  /* At this point the closure for FUNCTION and the actuals are pushed
     in the stack.  Just call the bloody function.  */

  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_CALL);
}
PKL_PHASE_END_HANDLER

/*
 * FUNC
 * | [TYPE]
 * | [FUNC_ARG]...
 * | BODY
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_func)
{
  /* This is a function prologue.  */
  pkl_asm_note (PKL_GEN_ASM,
                PKL_AST_FUNC_NAME (PKL_PASS_NODE));
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PROLOG);
  
  /* Push the function environment, for the arguments.  The
     compound-statement that is the body for the function will create
     it's own frame.  */
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSHF);
}
PKL_PHASE_END_HANDLER

/*
 * FUNC_ARG
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_func_arg)
{
  /* Pop an actual argument from the stack and put it in the current
     environment.  */

  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_REGVAR);
}
PKL_PHASE_END_HANDLER

/*
 * FUNC_ARG_TYPE
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_func_arg_type)
{
  /* Nothing to do here.  */
}
PKL_PHASE_END_HANDLER

/*
 * | [TYPE]
 * | [FUNC_ARG]...
 * | BODY
 * FUNC
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_func)
{
  /* Function epilogue.  */

  pkl_ast_node function = PKL_PASS_NODE;
  pkl_ast_node function_type = PKL_AST_TYPE (function);


  /* If the function returns a value, it is a run-time error to
     reach this point.  */

  /* XXX  */
  
  /* In a void function, return PVM_NULL in the stack.  */
  if (!PKL_AST_TYPE_F_RTYPE (function_type))
    pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSH, PVM_NULL);

  /* Pop the function's environment and return.  */
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_POPF, 1);
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_RETURN);
}
PKL_PHASE_END_HANDLER

/*
 * INTEGER
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_integer)
{
  pkl_ast_node integer = PKL_PASS_NODE;
  pkl_ast_node type;
  pvm_val val;
  int size;
  uint64_t value;

  type = PKL_AST_TYPE (integer);
  assert (type != NULL
          && PKL_AST_TYPE_CODE (type) == PKL_TYPE_INTEGRAL);

  size = PKL_AST_TYPE_I_SIZE (type);
  value = PKL_AST_INTEGER_VALUE (integer);

  if ((size - 1) & ~0x1f)
    {
      if (PKL_AST_TYPE_I_SIGNED (type))
        val = pvm_make_long (value, size);
      else
        val = pvm_make_ulong (value, size);
    }
  else
    {
      if (PKL_AST_TYPE_I_SIGNED (type))
        val = pvm_make_int (value, size);
      else
        val = pvm_make_uint (value, size);
    }
  
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSH, val);
}
PKL_PHASE_END_HANDLER

/*
 * IDENTIFIER
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_identifier)
{
  pkl_ast_node identifier = PKL_PASS_NODE;
  pvm_val val
    = pvm_make_string (PKL_AST_IDENTIFIER_POINTER (identifier));

  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSH, val);
}
PKL_PHASE_END_HANDLER

/*
 * STRING
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_string)
{
  pkl_ast_node string = PKL_PASS_NODE;
  pvm_val val
    = pvm_make_string (PKL_AST_STRING_POINTER (string));

  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSH, val);
}
PKL_PHASE_END_HANDLER

/*
 * TYPE
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_type)
{
  /* Avoid generating type nodes in certain circumstances.  */
  if (PKL_PASS_PARENT
      && (PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_STRUCT
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_INTEGER
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_STRING
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_OFFSET
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_FUNC
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_FUNC_ARG
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_DECL
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_VAR
          || PKL_AST_CODE (PKL_PASS_PARENT) == PKL_AST_MAP))
    PKL_PASS_BREAK;
}
PKL_PHASE_END_HANDLER

/*
 * TYPE_OFFSET
 * | BASE_TYPE
 * | UNIT
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_type_offset)
{
  /* We do not need to generate code for the offset type.  */
  PKL_PASS_BREAK;
}
PKL_PHASE_END_HANDLER

/*
 * | TYPE
 * | MAGNITUDE
 * | UNIT
 * OFFSET
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_offset)
{
  pkl_asm pasm = PKL_GEN_ASM;

  pkl_asm_insn (pasm, PKL_INSN_MKO);
}
PKL_PHASE_END_HANDLER

/*
 * | EXP
 * CAST
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_cast)
{
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node node = PKL_PASS_NODE;

  pkl_ast_node exp;
  pkl_ast_node to_type;
  pkl_ast_node from_type;

  exp = PKL_AST_CAST_EXP (node);

  to_type = PKL_AST_CAST_TYPE (node);
  from_type = PKL_AST_TYPE (exp);
  
  if (PKL_AST_TYPE_CODE (from_type) == PKL_TYPE_INTEGRAL
      && PKL_AST_TYPE_CODE (to_type) == PKL_TYPE_INTEGRAL)
    {
      pkl_asm_insn (pasm, PKL_INSN_NTON,
                    from_type, to_type);
    }
  else if (PKL_AST_TYPE_CODE (from_type) == PKL_TYPE_OFFSET
           && PKL_AST_TYPE_CODE (to_type) == PKL_TYPE_OFFSET)
    {
      pkl_ast_node from_base_type = PKL_AST_TYPE_O_BASE_TYPE (from_type);
      pkl_ast_node from_base_unit = PKL_AST_TYPE_O_UNIT (from_type);
      pkl_ast_node from_base_unit_type = PKL_AST_TYPE (from_base_unit);

      pkl_ast_node to_base_type = PKL_AST_TYPE_O_BASE_TYPE (to_type);
      pkl_ast_node to_base_unit = PKL_AST_TYPE_O_UNIT (to_type);
      pkl_ast_node to_base_unit_type = PKL_AST_TYPE (to_base_unit);

      /* Get the magnitude of the offset, cast it to the new base type
         and convert to new unit.  */
      /* XXX: use OGETMC here.  */
      /* XXX: we have to do the arithmetic in base_unit_types, then
         convert to to_base_type, to assure that to_base_type can hold
         the to_base_unit.  Otherwise weird division by zero occurs.  */
      pkl_asm_insn (pasm, PKL_INSN_OGETM);
      pkl_asm_insn (pasm, PKL_INSN_NTON,
                    from_base_type, to_base_type);

      PKL_PASS_SUBPASS (from_base_unit);
      pkl_asm_insn (pasm, PKL_INSN_NTON,
                    from_base_unit_type, to_base_type);

      pkl_asm_insn (pasm, PKL_INSN_MUL, to_base_type);

      PKL_PASS_SUBPASS (to_base_unit);
      pkl_asm_insn (pasm, PKL_INSN_NTON,
                    to_base_unit_type, to_base_type);

      pkl_asm_insn (pasm, PKL_INSN_DIV, to_base_type);
      pkl_asm_insn (pasm, PKL_INSN_SWAP);

      /* Push the new unit.  */
      PKL_PASS_SUBPASS (to_base_unit);
      pkl_asm_insn (pasm, PKL_INSN_SWAP);

      /* Get rid of the original offset.  */
      pkl_asm_insn (pasm, PKL_INSN_DROP);
      /* And create the new one.  */
      pkl_asm_insn (pasm, PKL_INSN_MKO);
    }
  else
    /* XXX: handle casts to structs and arrays.  For structs,
       reorder fields.  */
    assert (0);

}
PKL_PHASE_END_HANDLER

/*
 * | MAP_OFFSET
 * MAP
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_map)
{
  pkl_asm pasm = PKL_GEN_ASM;

  pkl_ast_node map = PKL_PASS_NODE;
  pkl_ast_node map_type = PKL_AST_MAP_TYPE (map);

  switch (PKL_AST_TYPE_CODE (map_type))
    {
    case PKL_TYPE_INTEGRAL:
    case PKL_TYPE_STRING:
      pkl_asm_insn (pasm, PKL_INSN_PEEK, map_type);
      break;
    case PKL_TYPE_OFFSET:
      pkl_asm_insn (pasm, PKL_INSN_PEEK,
                    PKL_AST_TYPE_O_BASE_TYPE (map_type));
      PKL_PASS_SUBPASS (PKL_AST_TYPE_O_UNIT (map_type));
      pkl_asm_insn (pasm, PKL_INSN_MKO);
      break;
    case PKL_TYPE_ARRAY:
      /* Generate code to create an array of maps of the given
         type.  */
      /* Handle:

         - If the size of the array is known, use a constant for loop.
         - If the size of the array is variable, use a for loop.
         - If the size of the array is not specified at all ([]) then
           use a while (not EOF) loop to create the array.
      */
    case PKL_TYPE_STRUCT:
      /* Generate code to create a struct of maps of the appropriate
         types.  */
    default:
      pkl_ice (PKL_PASS_AST, PKL_AST_LOC (map_type),
               "unhandled node type in codegen for node map #%" PRIu64,
               PKL_AST_UID (map));
      break;
    }
}
PKL_PHASE_END_HANDLER

/*
 * | ARRAY_INITIALIZER_INDEX
 * | ARRAY_INITIALIZER_EXP
 * ARRAY_INITIALIZER
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_array_initializer)
{
  /* Nothing to do.  */
}
PKL_PHASE_END_HANDLER

/*
 *  | ARRAY_TYPE
 *  | ARRAY_INITIALIZER
 *  | ...
 *  ARRAY
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_array)
{
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node node = PKL_PASS_NODE;

  pkl_asm_insn (pasm, PKL_INSN_PUSH,
                pvm_make_ulong (PKL_AST_ARRAY_NELEM (node), 64));

  pkl_asm_insn (pasm, PKL_INSN_PUSH,
                pvm_make_ulong (PKL_AST_ARRAY_NINITIALIZER (node), 64));

  pkl_asm_insn (pasm, PKL_INSN_MKA);
}
PKL_PHASE_END_HANDLER

/*
 * | ARRAY_REF_ARRAY
 * | ARRAY_REF_INDEX
 * ARRAY_REF
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_array_ref)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_AREF);
}
PKL_PHASE_END_HANDLER

/*
 *  | STRUCT_ELEM
 *  | ...
 *  STRUCT
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_struct)
{
  pkl_asm pasm = PKL_GEN_ASM;

  pkl_asm_insn (pasm, PKL_INSN_PUSH,
                pvm_make_ulong (PKL_AST_STRUCT_NELEM (PKL_PASS_NODE), 64));

  pkl_asm_insn (pasm, PKL_INSN_MKSCT);
}
PKL_PHASE_END_HANDLER

/*
 *  STRUCT_ELEM
 *  | [STRUCT_ELEM_NAME]
 *  | STRUCT_ELEM_EXP
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_struct_elem)
{
  /* If the struct initializer doesn't include a name, generate a null
     value as expected by the mksct instruction.  */
  if (!PKL_AST_STRUCT_ELEM_NAME (PKL_PASS_NODE))
    pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSH, PVM_NULL);
}
PKL_PHASE_END_HANDLER

/*
 * | STRUCT
 * | IDENTIFIER
 * STRUCT_REF
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_struct_ref)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_SREF);
}
PKL_PHASE_END_HANDLER

/*
 * (PKL_AST_ARRAY, PKL_AST_OFFSET, PKL_AST_TYPE,
    PKL_AST_STRUCT_ELEM_TYPE)
 * | TYPE_INTEGRAL
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_type_integral)
  PKL_PHASE_PARENT (4,
                    PKL_AST_ARRAY,
                    PKL_AST_OFFSET,
                    PKL_AST_TYPE,
                    PKL_AST_STRUCT_ELEM_TYPE)
{
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node node = PKL_PASS_NODE;

  pkl_asm_insn (pasm, PKL_INSN_PUSH,
                pvm_make_ulong (PKL_AST_TYPE_I_SIZE (node), 64));

  pkl_asm_insn (pasm, PKL_INSN_PUSH,
                pvm_make_uint (PKL_AST_TYPE_I_SIGNED (node), 32));

  pkl_asm_insn (pasm, PKL_INSN_MKTYI);
}
PKL_PHASE_END_HANDLER

/*
 * (PKL_AST_ARRAY, PKL_AST_OFFSET, PKL_AST_TYPE,
    PKL_AST_STRUCT_ELEM_TYPE)
 * | | ETYPE
 * | | NELEM
 * | TYPE_ARRAY
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_type_array)
  PKL_PHASE_PARENT (4,
                    PKL_AST_ARRAY,
                    PKL_AST_OFFSET,
                    PKL_AST_TYPE,
                    PKL_AST_STRUCT_ELEM_TYPE)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_MKTYA);
}
PKL_PHASE_END_HANDLER

/*
 * (PKL_AST_ARRAY, PKL_AST_OFFSET, PKL_AST_TYPE,
 *  PKL_AST_STRUCT_ELEM_TYPE)
 * | TYPE_STRING
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_type_string)
  PKL_PHASE_PARENT (4,
                    PKL_AST_ARRAY,
                    PKL_AST_OFFSET,
                    PKL_AST_TYPE,
                    PKL_AST_STRUCT_ELEM_TYPE)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_MKTYS);
}
PKL_PHASE_END_HANDLER

#if 0
/*
 * (PKL_AST_ARRAY, PKL_AST_OFFSET, PKL_AST_TYPE,
 *  PKL_AST_STRUCT_ELEM_TYPE)
 * | | BASE_TYPE
 * | | UNIT
 * | TYPE_OFFSET
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_type_offset)
  PKL_PHASE_PARENT (4,
                    PKL_AST_ARRAY,
                    PKL_AST_OFFSET,
                    PKL_AST_TYPE,
                    PKL_AST_STRUCT_ELEM_TYPE)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_MKTYO);
}
PKL_PHASE_END_HANDLER
#endif

/*
 * TYPE_STRUCT
 * | STRUCT_ELEM_TYPE
 * | ...
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_type_struct)
{
  /* Push a frame to the environment.  */
  /* XXX */
}
PKL_PHASE_END_HANDLER

/*
 * (PKL_AST_ARRAY, PKL_AST_OFFSET, PKL_AST_TYPE,
 *  PKL_AST_STRUCT_ELEM_TYPE)
 * | | STRUCT_ELEM_TYPE
 * | | ...
 * | TYPE_STRUCT
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_type_struct)
  PKL_PHASE_PARENT (4,
                    PKL_AST_ARRAY,
                    PKL_AST_OFFSET,
                    PKL_AST_TYPE,
                    PKL_AST_STRUCT_ELEM_TYPE)
{
 pkl_asm pasm = PKL_GEN_ASM;

 pkl_asm_insn (pasm, PKL_INSN_PUSH,
               pvm_make_ulong (PKL_AST_TYPE_S_NELEM (PKL_PASS_NODE), 64));
 pkl_asm_insn (pasm, PKL_INSN_MKTYSCT);

 /* XXX: pop N+1 frames from the environment.  */
}
PKL_PHASE_END_HANDLER

/*
 * (PKL_AST_ARRAY, PKL_AST_OFFSET, PKL_AST_TYPE,
 *  PKL_AST_STRUCT_ELEM_TYPE)
 * | STRUCT_ELEM_TYPE
 * | | [STRUCT_ELEM_TYPE_NAME]
 * | | STRUCT_ELEM_TYPE_TYPE
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_pr_struct_elem_type)
  PKL_PHASE_PARENT (4,
                    PKL_AST_ARRAY,
                    PKL_AST_OFFSET,
                    PKL_AST_TYPE,
                    PKL_AST_STRUCT_ELEM_TYPE)
{
  /* If the struct type element doesn't include a name, generate a
     null value as expected by the mktysct instruction.  */
  if (!PKL_AST_STRUCT_ELEM_TYPE_NAME (PKL_PASS_NODE))
    pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_PUSH, PVM_NULL);
}
PKL_PHASE_END_HANDLER

/* 
 * Expression handlers.
 *
 * | OPERAND1
 * | [OPERAND2]
 * EXP
 */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_add)
{
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node node = PKL_PASS_NODE;
  pkl_ast_node type = PKL_AST_TYPE (node);

  switch (PKL_AST_TYPE_CODE (type))
    {
    case PKL_TYPE_INTEGRAL:
      pkl_asm_insn (pasm, PKL_INSN_ADD, type);
      break;
    case PKL_TYPE_STRING:
      pkl_asm_insn (pasm, PKL_INSN_SCONC);
      break;
    case PKL_TYPE_OFFSET:
      {
        /* Calculate the magnitude of the new offset, which is the
           addition of both magnitudes.  The unit used for the result
           is the greatest common divisor of the operand's units.

           Note that since addition is commutative we can process OFF2
           first and save a swap.  */

        pkl_ast_node op1 = PKL_AST_EXP_OPERAND (node, 0);
        pkl_ast_node op1_type = PKL_AST_TYPE (op1);
        
        pkl_ast_node op2 = PKL_AST_EXP_OPERAND (node, 1);
        pkl_ast_node op2_type = PKL_AST_TYPE (op2);
        
        pkl_ast_node base_type = PKL_AST_TYPE_O_BASE_TYPE (type);

        PKL_PASS_SUBPASS (PKL_AST_TYPE_O_UNIT (op1_type));
        PKL_PASS_SUBPASS (PKL_AST_TYPE_O_UNIT (op2_type));
        pkl_asm_call (pasm, "_pkl_gcd");
        pkl_asm_insn (pasm, PKL_INSN_OGETMC, base_type);
        pkl_asm_insn (pasm, PKL_INSN_NIP);
        pkl_asm_insn (pasm, PKL_INSN_SWAP);

        PKL_PASS_SUBPASS (PKL_AST_TYPE_O_UNIT (op1_type));
        PKL_PASS_SUBPASS (PKL_AST_TYPE_O_UNIT (op2_type));
        pkl_asm_call (pasm, "_pkl_gcd");
        pkl_asm_insn (pasm, PKL_INSN_OGETMC, base_type);
        pkl_asm_insn (pasm, PKL_INSN_NIP);
        pkl_asm_insn (pasm, PKL_INSN_ADD, base_type);

        /* PKL_PASS_SUBPASS (res_unit); */
        PKL_PASS_SUBPASS (PKL_AST_TYPE_O_UNIT (op1_type));
        PKL_PASS_SUBPASS (PKL_AST_TYPE_O_UNIT (op2_type));
        pkl_asm_call (pasm, "_pkl_gcd");
        pkl_asm_insn (pasm, PKL_INSN_MKO);
      }
      break;
    default:
      assert (0);
      break;
    }
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_sub)
{
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node node = PKL_PASS_NODE;
  pkl_ast_node type = PKL_AST_TYPE (node);

  switch (PKL_AST_TYPE_CODE (type))
    {
    case PKL_TYPE_INTEGRAL:
      pkl_asm_insn (pasm, PKL_INSN_SUB, type);
      break;
    case PKL_TYPE_OFFSET:
      {
        /* Calculate the magnitude of the new offset, which is the
           subtraction of both magnitudes, once normalized to bits. */

        pkl_ast_node base_type = PKL_AST_TYPE_O_BASE_TYPE (type);
        pkl_ast_node res_unit = PKL_AST_TYPE_O_UNIT (type);

        pkl_asm_insn (pasm, PKL_INSN_SWAP);

        PKL_PASS_SUBPASS (res_unit);
        pkl_asm_insn (pasm, PKL_INSN_OGETMC, base_type);
        pkl_asm_insn (pasm, PKL_INSN_NIP);
        pkl_asm_insn (pasm, PKL_INSN_SWAP);
        PKL_PASS_SUBPASS (res_unit);
        pkl_asm_insn (pasm, PKL_INSN_OGETMC, base_type);
        pkl_asm_insn (pasm, PKL_INSN_NIP);
        pkl_asm_insn (pasm, PKL_INSN_SUB, base_type);

        PKL_PASS_SUBPASS (res_unit);
        pkl_asm_insn (pasm, PKL_INSN_MKO);
      }
      break;
    default:
      assert (0);
      break;
    }
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_mul)
{
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node node = PKL_PASS_NODE;
  pkl_ast_node type = PKL_AST_TYPE (node);

  switch (PKL_AST_TYPE_CODE (type))
    {
    case PKL_TYPE_INTEGRAL:
      pkl_asm_insn (pasm, PKL_INSN_MUL, type);
      break;
    case PKL_TYPE_OFFSET:
      {       
        pkl_ast_node op1 = PKL_AST_EXP_OPERAND (node, 0);
        pkl_ast_node op2 = PKL_AST_EXP_OPERAND (node, 1);
        pkl_ast_node op1_type = PKL_AST_TYPE (op1);
        pkl_ast_node op2_type = PKL_AST_TYPE (op2);
        int op1_type_code = PKL_AST_TYPE_CODE (op1_type);
        int op2_type_code = PKL_AST_TYPE_CODE (op2_type);

        pkl_ast_node offset_type, offset_unit, base_type;
        pkl_ast_node offset_op = NULL;

        /* The operation is commutative, so there is no need to swap
           the arguments.  */

        if (op2_type_code == PKL_TYPE_OFFSET)
          {
            pkl_asm_insn (pasm, PKL_INSN_OGETM);
            pkl_asm_insn (pasm, PKL_INSN_NIP);

            offset_op = op2;
          }

        pkl_asm_insn (pasm, PKL_INSN_SWAP);

        if (op1_type_code == PKL_TYPE_OFFSET)
          {
            pkl_asm_insn (pasm, PKL_INSN_OGETM);
            pkl_asm_insn (pasm, PKL_INSN_NIP);

            offset_op = op1;
          }

        assert (offset_op != NULL);
        offset_type = PKL_AST_TYPE (offset_op);
        offset_unit = PKL_AST_TYPE_O_UNIT (offset_type);
        base_type = PKL_AST_TYPE_O_BASE_TYPE (offset_type);

        pkl_asm_insn (pasm, PKL_INSN_MUL, base_type);
          
        PKL_PASS_SUBPASS (offset_unit);
        pkl_asm_insn (pasm, PKL_INSN_MKO);
      }
      break;
    default:
      assert (0);
      break;
    }
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_div)
{
  pkl_ast_node node = PKL_PASS_NODE;
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node type = PKL_AST_TYPE (node);
  pkl_ast_node op2 = PKL_AST_EXP_OPERAND (node, 0);
  pkl_ast_node op2_type = PKL_AST_TYPE (op2);

  switch (PKL_AST_TYPE_CODE (type))
    {
    case PKL_TYPE_INTEGRAL:
      {
        if (PKL_AST_TYPE_CODE (op2_type) == PKL_TYPE_OFFSET)
          {
            /* Calculate the resulting integral value, which is the
               division of both magnitudes, once normalized to
               bits. */

            pkl_ast_node unit_type = pkl_ast_make_integral_type (PKL_PASS_AST, 64, 0);
            pkl_ast_node unit_bits = pkl_ast_make_integer (PKL_PASS_AST, 1);
            PKL_AST_TYPE (unit_bits) = ASTREF (unit_type);

            pkl_asm_insn (pasm, PKL_INSN_SWAP);

            PKL_PASS_SUBPASS (unit_bits);
            pkl_asm_insn (pasm, PKL_INSN_OGETMC, type);
            pkl_asm_insn (pasm, PKL_INSN_NIP);
            pkl_asm_insn (pasm, PKL_INSN_SWAP);
            PKL_PASS_SUBPASS (unit_bits);
            pkl_asm_insn (pasm, PKL_INSN_OGETMC, type);
            pkl_asm_insn (pasm, PKL_INSN_NIP);

            pkl_asm_insn (pasm, PKL_INSN_DIV, type);

            ASTREF (unit_bits); pkl_ast_node_free (unit_bits);
          }
        else
          pkl_asm_insn (pasm, PKL_INSN_DIV, type);
        break;
      }
    default:
      assert (0);
      break;
    }
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_mod)
{
  pkl_ast_node node = PKL_PASS_NODE;

  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node type = PKL_AST_TYPE (node);
  pkl_ast_node op1 = PKL_AST_EXP_OPERAND (node, 0);
  pkl_ast_node op1_type = PKL_AST_TYPE (op1);

  switch (PKL_AST_TYPE_CODE (type))
    {
    case PKL_TYPE_INTEGRAL:
      pkl_asm_insn (pasm, PKL_INSN_MOD, type);
      break;
    case PKL_TYPE_OFFSET:
      {
        /* Calculate the magnitude of the new offset, which is the
           modulus of both magnitudes, the second argument converted
           to first's units.  */

        pkl_ast_node base_type = PKL_AST_TYPE_O_BASE_TYPE (type);
        pkl_ast_node op1_unit = PKL_AST_TYPE_O_UNIT (op1_type);

        pkl_asm_insn (pasm, PKL_INSN_SWAP);

        pkl_asm_insn (pasm, PKL_INSN_OGETM);
        pkl_asm_insn (pasm, PKL_INSN_NIP);
        pkl_asm_insn (pasm, PKL_INSN_SWAP);
        PKL_PASS_SUBPASS (op1_unit);
        pkl_asm_insn (pasm, PKL_INSN_OGETMC, base_type);
        pkl_asm_insn (pasm, PKL_INSN_NIP);

        pkl_asm_insn (pasm, PKL_INSN_MOD, base_type);

        PKL_PASS_SUBPASS (op1_unit);
        pkl_asm_insn (pasm, PKL_INSN_MKO);
      }
      break;
    default:
      assert (0);
      break;
    }
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_intexp)
{
  pkl_asm pasm = PKL_GEN_ASM;

  pkl_ast_node node = PKL_PASS_NODE;
  pkl_ast_node type = PKL_AST_TYPE (node);

  enum pkl_asm_insn insn;

  switch (PKL_AST_EXP_CODE (node))
    {
    case PKL_AST_OP_BAND: insn = PKL_INSN_BAND; break;
    case PKL_AST_OP_BNOT: insn = PKL_INSN_BNOT; break;
    case PKL_AST_OP_NEG: insn = PKL_INSN_NEG; break;
    case PKL_AST_OP_IOR: insn = PKL_INSN_BOR; break;
    case PKL_AST_OP_XOR: insn = PKL_INSN_BXOR; break;
    case PKL_AST_OP_SL: insn = PKL_INSN_SL; break;
    case PKL_AST_OP_SR: insn = PKL_INSN_SR; break;
    default:
      assert (0);
      break;
    }
          
  switch (PKL_AST_TYPE_CODE (type))
    {
    case PKL_TYPE_INTEGRAL:
      pkl_asm_insn (pasm, insn, type);
      break;
    default:
      assert (0);
      break;
    }
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_and)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_AND);
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_or)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_OR);
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_not)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_NOT);
}
PKL_PHASE_END_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_rela)
{
  pkl_asm pasm = PKL_GEN_ASM;
  pkl_ast_node exp = PKL_PASS_NODE;
  int exp_code = PKL_AST_EXP_CODE (exp);
  pkl_ast_node op1 = PKL_AST_EXP_OPERAND (exp, 0);
  pkl_ast_node op1_type = PKL_AST_TYPE (op1);

  enum pkl_asm_insn rela_insn;

  switch (exp_code)
    {
    case PKL_AST_OP_EQ: rela_insn = PKL_INSN_EQ; break;
    case PKL_AST_OP_NE: rela_insn = PKL_INSN_NE; break;
    case PKL_AST_OP_LT: rela_insn = PKL_INSN_LT; break;
    case PKL_AST_OP_GT: rela_insn = PKL_INSN_GT; break;
    case PKL_AST_OP_LE: rela_insn = PKL_INSN_LE; break;
    case PKL_AST_OP_GE: rela_insn = PKL_INSN_GE; break;
    default:
      assert (0);
      break;
    }

  switch (PKL_AST_TYPE_CODE (op1_type))
    {
    case PKL_TYPE_INTEGRAL:
    case PKL_TYPE_STRING:
      pkl_asm_insn (pasm, rela_insn, op1_type);
      break;
    case PKL_TYPE_OFFSET:
      {
        /* Calculate the resulting integral value, which is the
           comparison of both magnitudes, once normalized to bits.
           Note that at this point the magnitude types of both offset
           operands are the same.  */

        pkl_ast_node base_type = PKL_AST_TYPE_O_BASE_TYPE (op1_type);
        pkl_ast_node unit_type = pkl_ast_make_integral_type (PKL_PASS_AST, 64, 0);
        pkl_ast_node unit_bits = pkl_ast_make_integer (PKL_PASS_AST, 1);
        PKL_AST_TYPE (unit_bits) = ASTREF (unit_type);

        /* Equality and inequality are commutative, so we can save an
           instruction here.  */
        if (exp_code != PKL_AST_OP_EQ && exp_code != PKL_AST_OP_NE)
          pkl_asm_insn (pasm, PKL_INSN_SWAP);

        PKL_PASS_SUBPASS (unit_bits);
        pkl_asm_insn (pasm, PKL_INSN_OGETMC, base_type);
        pkl_asm_insn (pasm, PKL_INSN_NIP);
        pkl_asm_insn (pasm, PKL_INSN_SWAP);
        PKL_PASS_SUBPASS (unit_bits);
        pkl_asm_insn (pasm, PKL_INSN_OGETMC, base_type);
        pkl_asm_insn (pasm, PKL_INSN_NIP);

        pkl_asm_insn (pasm, rela_insn, base_type);

        ASTREF (unit_bits); pkl_ast_node_free (unit_bits);
      }
      break;
    default:
      assert (0);
      break;
    }
}
PKL_PHASE_END_HANDLER

#undef RELA_EXP_HANDLER

PKL_PHASE_BEGIN_HANDLER (pkl_gen_ps_op_sizeof)
{
  pkl_asm_insn (PKL_GEN_ASM, PKL_INSN_SIZ);
}
PKL_PHASE_END_HANDLER

#undef BIN_INTEGRAL_EXP_HANDLER

/* The handler below generates and ICE if a given node isn't handled
   by the code generator.  */

PKL_PHASE_BEGIN_HANDLER (pkl_gen_noimpl)
{
  pkl_ast_node node = PKL_PASS_NODE;

  if (PKL_AST_CODE (node) == PKL_AST_EXP)
    {
      pkl_ice (PKL_PASS_AST, PKL_AST_LOC (node),
               "unhandled node #%" PRIu64 " with code %d opcode %d in code generator",
               PKL_AST_UID (node), PKL_AST_CODE (node), PKL_AST_EXP_CODE (node));
    }
  else if (PKL_AST_CODE (node) == PKL_AST_TYPE)
    {
      pkl_ice (PKL_PASS_AST, PKL_AST_LOC (node),
               "unhandled node #%" PRIu64 " with code %d typecode %d in code generator",
               PKL_AST_UID (node), PKL_AST_CODE (node), PKL_AST_TYPE_CODE (node));
    }
  else
    pkl_ice (PKL_PASS_AST, PKL_AST_LOC (node),
             "unhandled node #%" PRIu64 " with code %d in code generator",
             PKL_AST_UID (node), PKL_AST_CODE (node));

  PKL_PASS_ERROR;
}
PKL_PHASE_END_HANDLER

struct pkl_phase pkl_phase_gen =
  {
   PKL_PHASE_PR_HANDLER (PKL_AST_DECL, pkl_gen_pr_decl),
   PKL_PHASE_PS_HANDLER (PKL_AST_DECL, pkl_gen_ps_decl),
   PKL_PHASE_PS_HANDLER (PKL_AST_VAR, pkl_gen_ps_var),
   PKL_PHASE_PR_HANDLER (PKL_AST_COMP_STMT, pkl_gen_pr_comp_stmt),
   PKL_PHASE_PS_HANDLER (PKL_AST_COMP_STMT, pkl_gen_ps_comp_stmt),
   PKL_PHASE_PS_HANDLER (PKL_AST_NULL_STMT, pkl_gen_ps_null_stmt),
   PKL_PHASE_PS_HANDLER (PKL_AST_ASS_STMT, pkl_gen_ps_ass_stmt),
   PKL_PHASE_PR_HANDLER (PKL_AST_IF_STMT, pkl_gen_pr_if_stmt),
   PKL_PHASE_PR_HANDLER (PKL_AST_LOOP_STMT, pkl_gen_pr_loop_stmt),
   PKL_PHASE_PS_HANDLER (PKL_AST_RETURN_STMT, pkl_gen_ps_return_stmt),
   PKL_PHASE_PS_HANDLER (PKL_AST_EXP_STMT, pkl_gen_ps_exp_stmt),
   PKL_PHASE_PS_HANDLER (PKL_AST_FUNCALL, pkl_gen_ps_funcall),
   PKL_PHASE_PS_HANDLER (PKL_AST_FUNCALL_ARG, pkl_gen_ps_funcall_arg),
   PKL_PHASE_PR_HANDLER (PKL_AST_FUNC, pkl_gen_pr_func),
   PKL_PHASE_PS_HANDLER (PKL_AST_FUNC, pkl_gen_ps_func),
   PKL_PHASE_PS_HANDLER (PKL_AST_FUNC_ARG, pkl_gen_ps_func_arg),
   PKL_PHASE_PS_HANDLER (PKL_AST_FUNC_ARG_TYPE, pkl_gen_ps_func_arg_type),
   PKL_PHASE_PR_HANDLER (PKL_AST_TYPE, pkl_gen_pr_type),
   PKL_PHASE_PR_HANDLER (PKL_AST_PROGRAM, pkl_gen_pr_program),
   PKL_PHASE_PS_HANDLER (PKL_AST_PROGRAM, pkl_gen_ps_program),
   PKL_PHASE_PS_HANDLER (PKL_AST_INTEGER, pkl_gen_ps_integer),
   PKL_PHASE_PS_HANDLER (PKL_AST_IDENTIFIER, pkl_gen_ps_identifier),
   PKL_PHASE_PS_HANDLER (PKL_AST_STRING, pkl_gen_ps_string),
   PKL_PHASE_PR_TYPE_HANDLER (PKL_TYPE_OFFSET, pkl_gen_pr_type_offset),
   PKL_PHASE_PS_HANDLER (PKL_AST_OFFSET, pkl_gen_ps_offset),
   PKL_PHASE_PS_HANDLER (PKL_AST_CAST, pkl_gen_ps_cast),
   PKL_PHASE_PS_HANDLER (PKL_AST_MAP, pkl_gen_ps_map),
   PKL_PHASE_PS_HANDLER (PKL_AST_ARRAY, pkl_gen_ps_array),
   PKL_PHASE_PS_HANDLER (PKL_AST_ARRAY_REF, pkl_gen_ps_array_ref),
   PKL_PHASE_PR_HANDLER (PKL_AST_ARRAY_INITIALIZER, pkl_gen_ps_array_initializer),
   PKL_PHASE_PS_HANDLER (PKL_AST_STRUCT, pkl_gen_ps_struct),
   PKL_PHASE_PR_HANDLER (PKL_AST_STRUCT_ELEM, pkl_gen_pr_struct_elem),
   PKL_PHASE_PS_HANDLER (PKL_AST_STRUCT_REF, pkl_gen_ps_struct_ref),
   PKL_PHASE_PR_HANDLER (PKL_AST_STRUCT_ELEM_TYPE, pkl_gen_pr_struct_elem_type),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_ADD, pkl_gen_ps_op_add),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_SUB, pkl_gen_ps_op_sub),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_MUL, pkl_gen_ps_op_mul),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_MOD, pkl_gen_ps_op_mod),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_BAND, pkl_gen_ps_op_intexp),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_BNOT, pkl_gen_ps_op_intexp),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_NEG, pkl_gen_ps_op_intexp),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_IOR, pkl_gen_ps_op_intexp),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_XOR, pkl_gen_ps_op_intexp),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_SL, pkl_gen_ps_op_intexp),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_SR, pkl_gen_ps_op_intexp),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_DIV, pkl_gen_ps_op_div),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_AND, pkl_gen_ps_op_and),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_OR, pkl_gen_ps_op_or),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_NOT, pkl_gen_ps_op_not),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_EQ, pkl_gen_ps_op_rela),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_NE, pkl_gen_ps_op_rela),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_LT, pkl_gen_ps_op_rela),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_LE, pkl_gen_ps_op_rela),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_GT, pkl_gen_ps_op_rela),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_GE, pkl_gen_ps_op_rela),
   PKL_PHASE_PS_OP_HANDLER (PKL_AST_OP_SIZEOF, pkl_gen_ps_op_sizeof),
   PKL_PHASE_PS_TYPE_HANDLER (PKL_TYPE_INTEGRAL, pkl_gen_ps_type_integral),
   PKL_PHASE_PS_TYPE_HANDLER (PKL_TYPE_ARRAY, pkl_gen_ps_type_array),
   PKL_PHASE_PS_TYPE_HANDLER (PKL_TYPE_STRING, pkl_gen_ps_type_string),
   PKL_PHASE_PR_TYPE_HANDLER (PKL_TYPE_STRUCT, pkl_gen_pr_type_struct),
   PKL_PHASE_PS_TYPE_HANDLER (PKL_TYPE_STRUCT, pkl_gen_ps_type_struct),
   PKL_PHASE_ELSE_HANDLER (pkl_gen_noimpl),
  };
