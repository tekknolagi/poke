/* pvm-val.c - Values for the PVM.  */

/* Copyright (C) 2019, 2020 Jose E. Marchesi */

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
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "poke.h" /* XXX for poke_vm and poke_compiler, this should go
                     away.  */

#include "pkl-asm.h"
#include "pk-term.h"
#include "pvm.h"

#define STREQ(a, b) (strcmp (a, b) == 0)

pvm_val
pvm_make_int (int32_t value, int size)
{
  return (((((int64_t) value) & 0xffffffff) << 32)
          | (((size - 1) & 0x1f) << 3)
          | PVM_VAL_TAG_INT);
}

pvm_val
pvm_make_uint (uint32_t value, int size)
{
  return (((((uint64_t) value) & 0xffffffff) << 32)
          | (((size - 1) & 0x1f) << 3)
          | PVM_VAL_TAG_UINT);
}

static inline pvm_val
pvm_make_long_ulong (int64_t value, int size, int tag)
{
  uint64_t *ll = pvm_alloc (sizeof (uint64_t) * 2);

  ll[0] = value;
  ll[1] = (size - 1) & 0x3f;
  return ((uint64_t) (uintptr_t) ll) | tag;
}

pvm_val
pvm_make_long (int64_t value, int size)
{
  return pvm_make_long_ulong (value, size, PVM_VAL_TAG_LONG);
}

pvm_val
pvm_make_ulong (uint64_t value, int size)
{
  return pvm_make_long_ulong (value, size, PVM_VAL_TAG_ULONG);
}

static pvm_val_box
pvm_make_box (uint8_t tag)
{
  pvm_val_box box = pvm_alloc (sizeof (struct pvm_val_box));

  PVM_VAL_BOX_TAG (box) = tag;
  return box;
}

pvm_val
pvm_make_string (const char *str)
{
  pvm_val_box box = pvm_make_box (PVM_VAL_TAG_STR);

  PVM_VAL_BOX_STR (box) = pvm_alloc_strdup (str);
  return PVM_BOX (box);
}

pvm_val
pvm_make_array (pvm_val nelem, pvm_val type)
{
  pvm_val_box box = pvm_make_box (PVM_VAL_TAG_ARR);
  pvm_array arr = pvm_alloc (sizeof (struct pvm_array));
  size_t nbytes = sizeof (struct pvm_array_elem) * PVM_VAL_ULONG (nelem);
  size_t i;

  arr->ios = PVM_NULL;
  arr->offset = PVM_NULL;
  arr->elems_bound = PVM_NULL;
  arr->size_bound = PVM_NULL;
  arr->mapper = PVM_NULL;
  arr->writer = PVM_NULL;
  arr->nelem = nelem;
  arr->type = type;
  arr->elems = pvm_alloc (nbytes);

  for (i = 0; i < PVM_VAL_ULONG (nelem); ++i)
    {
      arr->elems[i].offset = PVM_NULL;
      arr->elems[i].value = PVM_NULL;
    }

  PVM_VAL_BOX_ARR (box) = arr;
  return PVM_BOX (box);
}

pvm_val
pvm_make_struct (pvm_val nfields, pvm_val nmethods, pvm_val type)
{
  pvm_val_box box = pvm_make_box (PVM_VAL_TAG_SCT);
  pvm_struct sct = pvm_alloc (sizeof (struct pvm_struct));
  size_t i;
  size_t nfieldbytes
    = sizeof (struct pvm_struct_field) * PVM_VAL_ULONG (nfields);
  size_t nmethodbytes
    = sizeof (struct pvm_struct_method) * PVM_VAL_ULONG (nmethods);

  sct->ios = PVM_NULL;
  sct->offset = PVM_NULL;
  sct->mapper = PVM_NULL;
  sct->writer = PVM_NULL;
  sct->type = type;

  sct->nfields = nfields;
  sct->fields = pvm_alloc (nfieldbytes);
  memset (sct->fields, 0, nfieldbytes);

  sct->nmethods = nmethods;
  sct->methods = pvm_alloc (nmethodbytes);
  memset (sct->methods, 0, nmethodbytes);

  for (i = 0; i < PVM_VAL_ULONG (sct->nfields); ++i)
    {
      sct->fields[i].offset = PVM_NULL;
      sct->fields[i].name = PVM_NULL;
      sct->fields[i].value = PVM_NULL;
      sct->fields[i].modified = pvm_make_int (0, 32);
    }

  for (i = 0; i < PVM_VAL_ULONG (sct->nmethods); ++i)
    {
      sct->methods[i].name = PVM_NULL;
      sct->methods[i].value = PVM_NULL;
    }

  PVM_VAL_BOX_SCT (box) = sct;
  return PVM_BOX (box);
}

pvm_val
pvm_ref_struct (pvm_val sct, pvm_val name)
{
  size_t nfields, nmethods, i;
  struct pvm_struct_field *fields;
  struct pvm_struct_method *methods;

  assert (PVM_IS_SCT (sct) && PVM_IS_STR (name));

  /* Lookup fields.  */
  nfields = PVM_VAL_ULONG (PVM_VAL_SCT_NFIELDS (sct));
  fields = PVM_VAL_SCT (sct)->fields;

  for (i = 0; i < nfields; ++i)
    {
      if (fields[i].name != PVM_NULL
          && STREQ (PVM_VAL_STR (fields[i].name),
                    PVM_VAL_STR (name)))
        return fields[i].value;
    }

  /* Lookup methods.  */
  nmethods = PVM_VAL_ULONG (PVM_VAL_SCT_NMETHODS (sct));
  methods = PVM_VAL_SCT (sct)->methods;

  for (i = 0; i < nmethods; ++i)
    {
      if (STREQ (PVM_VAL_STR (methods[i].name),
                 PVM_VAL_STR (name)))
        return methods[i].value;
    }

  return PVM_NULL;
}

int
pvm_set_struct (pvm_val sct, pvm_val name, pvm_val val)
{
  size_t nfields, i;
  struct pvm_struct_field *fields;

  assert (PVM_IS_SCT (sct) && PVM_IS_STR (name));

  nfields = PVM_VAL_ULONG (PVM_VAL_SCT_NFIELDS (sct));
  fields = PVM_VAL_SCT (sct)->fields;

  for (i = 0; i < nfields; ++i)
    {
      if (fields[i].name != PVM_NULL
          && STREQ (PVM_VAL_STR (fields[i].name),
                    PVM_VAL_STR (name)))
        {
          PVM_VAL_SCT_FIELD_VALUE (sct,i) = val;
          PVM_VAL_SCT_FIELD_MODIFIED (sct,i) =
            pvm_make_int (1, 32);
          return 1;
        }
    }

  return 0;
}

pvm_val
pvm_get_struct_method (pvm_val sct, const char *name)
{
  size_t i, nmethods = PVM_VAL_ULONG (PVM_VAL_SCT_NMETHODS (sct));
  struct pvm_struct_method *methods = PVM_VAL_SCT (sct)->methods;

  for (i = 0; i < nmethods; ++i)
    {
      if (STREQ (PVM_VAL_STR (methods[i].name), name))
        return methods[i].value;
    }

  return PVM_NULL;
}

static pvm_val
pvm_make_type (enum pvm_type_code code)
{
  pvm_val_box box = pvm_make_box (PVM_VAL_TAG_TYP);
  pvm_type type = pvm_alloc (sizeof (struct pvm_type));

  memset (type, 0, sizeof (struct pvm_type));
  type->code = code;

  PVM_VAL_BOX_TYP (box) = type;
  return PVM_BOX (box);
}

pvm_val
pvm_make_integral_type (pvm_val size, pvm_val signed_p)
{
  pvm_val itype = pvm_make_type (PVM_TYPE_INTEGRAL);

  PVM_VAL_TYP_I_SIZE (itype) = size;
  PVM_VAL_TYP_I_SIGNED (itype) = signed_p;
  return itype;
}

pvm_val
pvm_make_string_type (void)
{
  return pvm_make_type (PVM_TYPE_STRING);
}

pvm_val
pvm_make_any_type (void)
{
  return pvm_make_type (PVM_TYPE_ANY);
}

pvm_val
pvm_make_offset_type (pvm_val base_type, pvm_val unit)
{
  pvm_val otype = pvm_make_type (PVM_TYPE_OFFSET);

  PVM_VAL_TYP_O_BASE_TYPE (otype) = base_type;
  PVM_VAL_TYP_O_UNIT (otype) = unit;
  return otype;
}

pvm_val
pvm_make_array_type (pvm_val type, pvm_val bound)
{
  pvm_val atype = pvm_make_type (PVM_TYPE_ARRAY);

  PVM_VAL_TYP_A_ETYPE (atype) = type;
  PVM_VAL_TYP_A_BOUND (atype) = bound;
  return atype;
}

pvm_val
pvm_make_struct_type (pvm_val nfields, pvm_val name,
                      pvm_val *fnames, pvm_val *ftypes)
{
  pvm_val stype = pvm_make_type (PVM_TYPE_STRUCT);

  PVM_VAL_TYP_S_NAME (stype) = name;
  PVM_VAL_TYP_S_NFIELDS (stype) = nfields;
  PVM_VAL_TYP_S_FNAMES (stype) = fnames;
  PVM_VAL_TYP_S_FTYPES (stype) = ftypes;

  return stype;
}

pvm_val
pvm_make_closure_type (pvm_val rtype,
                       pvm_val nargs, pvm_val *atypes)
{
  pvm_val ctype = pvm_make_type (PVM_TYPE_CLOSURE);

  PVM_VAL_TYP_C_RETURN_TYPE (ctype) = rtype;
  PVM_VAL_TYP_C_NARGS (ctype) = nargs;
  PVM_VAL_TYP_C_ATYPES (ctype) = atypes;

  return ctype;
}

pvm_val
pvm_make_cls (pvm_routine routine, void **pointers)
{
  pvm_val_box box = pvm_make_box (PVM_VAL_TAG_CLS);
  pvm_cls cls = pvm_alloc_cls ();

  cls->routine = routine;
  cls->pointers = pointers;
  cls->entry_point = PVM_ROUTINE_BEGINNING (routine);
  cls->env = NULL; /* This should be set by a PEC instruction before
                      using the closure.  */

  PVM_VAL_BOX_CLS (box) = cls;
  return PVM_BOX (box);
}

pvm_val
pvm_make_offset (pvm_val magnitude, pvm_val unit)
{
  pvm_val_box box = pvm_make_box (PVM_VAL_TAG_OFF);
  pvm_off off = pvm_alloc (sizeof (struct pvm_off));

  off->base_type = pvm_typeof (magnitude);
  off->magnitude = magnitude;
  off->unit = unit;

  PVM_VAL_BOX_OFF (box) = off;
  return PVM_BOX (box);
}

void
pvm_allocate_struct_attrs (pvm_val nfields,
                           pvm_val **fnames, pvm_val **ftypes)
{
  size_t nbytes = sizeof (pvm_val) * PVM_VAL_ULONG (nfields) * 2;
  *fnames = pvm_alloc (nbytes);
  *ftypes = pvm_alloc (nbytes);
}

void
pvm_allocate_closure_attrs (pvm_val nargs, pvm_val **atypes)
{
  size_t nbytes = sizeof (pvm_val) * PVM_VAL_ULONG (nargs);
  *atypes = pvm_alloc (nbytes);
}

pvm_val
pvm_elemsof (pvm_val val)
{
  if (PVM_IS_ARR (val))
    return PVM_VAL_ARR_NELEM (val);
  else if (PVM_IS_SCT (val))
    return PVM_VAL_SCT_NFIELDS (val);
  else if (PVM_IS_STR (val))
    return pvm_make_ulong (strlen (PVM_VAL_STR (val)), 64);
  else
    return pvm_make_ulong (1, 64);
}

pvm_val
pvm_val_mapper (pvm_val val)
{
  if (PVM_IS_ARR (val))
    return PVM_VAL_ARR_MAPPER (val);
  if (PVM_IS_SCT (val))
    return PVM_VAL_SCT_MAPPER (val);

  return PVM_NULL;
}

pvm_val
pvm_val_writer (pvm_val val)
{
  if (PVM_IS_ARR (val))
    return PVM_VAL_ARR_WRITER (val);
  if (PVM_IS_SCT (val))
    return PVM_VAL_SCT_WRITER (val);

  return PVM_NULL;
}

uint64_t
pvm_sizeof (pvm_val val)
{
  if (PVM_IS_INT (val))
    return PVM_VAL_INT_SIZE (val);
  else if (PVM_IS_UINT (val))
    return PVM_VAL_UINT_SIZE (val);
  else if (PVM_IS_LONG (val))
    return PVM_VAL_LONG_SIZE (val);
  else if (PVM_IS_ULONG (val))
    return PVM_VAL_ULONG_SIZE (val);
  else if (PVM_IS_STR (val))
    return (strlen (PVM_VAL_STR (val)) + 1) * 8;
  else if (PVM_IS_ARR (val))
    {
      size_t nelem, i;
      size_t size = 0;

      nelem = PVM_VAL_ULONG (PVM_VAL_ARR_NELEM (val));
      for (i = 0; i < nelem; ++i)
        size += pvm_sizeof (PVM_VAL_ARR_ELEM_VALUE (val, i));

      return size;
    }
  else if (PVM_IS_SCT (val))
    {
      pvm_val sct_offset = PVM_VAL_SCT_OFFSET (val);
      size_t nfields, i, size, sct_offset_bits;

      if (sct_offset == PVM_NULL)
        sct_offset_bits = 0;
      else
        sct_offset_bits = PVM_VAL_ULONG (sct_offset);

      nfields = PVM_VAL_ULONG (PVM_VAL_SCT_NFIELDS (val));

      size = 0;
      for (i = 0; i < nfields; ++i)
        {
          pvm_val elem_value = PVM_VAL_SCT_FIELD_VALUE (val, i);
          pvm_val elem_offset = PVM_VAL_SCT_FIELD_OFFSET (val, i);
          uint64_t elem_size_bits = pvm_sizeof (elem_value);
          uint64_t elem_offset_bits;

          if (elem_offset == PVM_NULL)
            size += elem_size_bits;
          else
            {
              elem_offset_bits = PVM_VAL_ULONG (elem_offset);

#define MAX(A,B) ((A) > (B) ? (A) : (B))
              size = MAX (size, elem_offset_bits - sct_offset_bits + elem_size_bits);
            }
        }

      return size;
    }
  else if (PVM_IS_OFF (val))
    return pvm_sizeof (PVM_VAL_OFF_MAGNITUDE (val));
  else if (PVM_IS_TYP (val))
    {
      size_t size;

      /* XXX */
      assert (0);
      switch (PVM_VAL_TYP_CODE (val))
        {
        case PVM_TYPE_INTEGRAL:
          size = PVM_VAL_ULONG (PVM_VAL_TYP_I_SIZE (val));
          break;
        default:
          assert (0);
        };

      return size;
    }

  assert (0);
  return 0;
}

/* XXX use similar printers for hexadecimal and octal, so we can use
   _'s */

static void
pvm_print_binary (uint64_t val, int size, int sign)
{
  char b[65];

  if (size != 64 && size != 32 && size != 16 && size != 8
      && size != 4)
    pk_printf ("(%sint<%d>) ", sign ? "" : "u", size);

  for (int z = 0; z < size; z++) {
    b[size-1-z] = ((val >> z) & 0x1) + '0';
  }
  b[size] = '\0';

  pk_printf ("0b%s", b);

  if (size == 64)
    {
      if (!sign)
        pk_puts ("U");
      pk_puts ("L");
    }
  else if (size == 16)
    {
      if (!sign)
        pk_puts ("U");
      pk_puts ("H");
    }
  else if (size == 8)
    {
      if (!sign)
        pk_puts ("U");
      pk_puts ("B");
    }
  else if (size == 4)
    {
      {
        if (!sign)
          pk_puts ("U");
      }
      pk_puts ("N");
    }
}

void
pvm_print_val (pvm_val val, int base, int flags)
{
  const char *long64_fmt, *long_fmt;
  const char *ulong64_fmt, *ulong_fmt;
  const char *int32_fmt, *int16_fmt, *int8_fmt, *int4_fmt, *int_fmt;
  const char *uint32_fmt, *uint16_fmt, *uint8_fmt, *uint4_fmt, *uint_fmt;

  /* Select the appropriate formatting templates for the given
     base.  */
  switch (base)
    {
    case 8:
      long64_fmt = "0o%" PRIo64 "L";
      long_fmt = "(int<%d>) 0o%" PRIo64;
      ulong64_fmt = "0o%" PRIo64 "UL";
      ulong_fmt = "(uint<%d>) 0o%" PRIo64;
      int32_fmt = "0o%" PRIo32;
      int16_fmt = "0o%" PRIo32 "H";
      int8_fmt = "0o%" PRIo32 "B";
      int4_fmt = "0o%" PRIo32 "N";
      int_fmt = "(int<%d>) 0o%" PRIo32;
      uint32_fmt = "0o%" PRIo32 "U";
      uint16_fmt = "0o%" PRIo32 "UH";
      uint8_fmt = "0o%" PRIo32 "UB";
      uint4_fmt = "0o%" PRIo32 "UN";
      uint_fmt = "(uint<%d>) 0o%" PRIo32;
      break;
    case 10:
      long64_fmt = "%" PRIi64 "L";
      long_fmt = "(int<%d>) %" PRIi64;
      ulong64_fmt = "%" PRIu64 "UL";
      ulong_fmt = "(uint<%d>) %" PRIu64;
      int32_fmt = "%" PRIi32;
      int16_fmt = "%" PRIi32 "H";
      int8_fmt = "%" PRIi32 "B";
      int4_fmt = "%" PRIi32 "N";
      int_fmt = "(int<%d>) %" PRIi32;
      uint32_fmt = "%" PRIu32 "U";
      uint16_fmt = "%" PRIu32 "UH";
      uint8_fmt = "%" PRIu32 "UB";
      uint4_fmt = "%" PRIu32 "UN";
      uint_fmt = "(uint<%d>) %" PRIu32;
      break;
    case 16:
      long64_fmt = "0x%" PRIx64 "L";
      long_fmt = "(int<%d>) %" PRIx64;
      ulong64_fmt = "0x%" PRIx64 "UL";
      ulong_fmt = "(uint<%d>) %" PRIx64;
      int32_fmt = "0x%" PRIx32;
      int16_fmt = "0x%" PRIx32 "H";
      int8_fmt = "0x%" PRIx32 "B";
      int4_fmt = "0x%" PRIx32 "N";
      int_fmt = "(int<%d>) 0x%" PRIx32;
      uint32_fmt = "0x%" PRIx32 "U";
      uint16_fmt = "0x%" PRIx32 "UH";
      uint8_fmt = "0x%" PRIx32 "UB";
      uint4_fmt = "0x%" PRIx32 "UN";
      uint_fmt = "(uint<%d>) 0x%" PRIo32;
      break;
    case 2:
      /* This base doesn't use printf's formatting strings, but its
         own printer.  */
      long64_fmt = "";
      long_fmt = "";
      ulong64_fmt = "";
      ulong_fmt = "";
      int32_fmt = "";
      int16_fmt = "";
      int8_fmt = "";
      int4_fmt = "";
      int_fmt = "";
      uint32_fmt = "";
      uint16_fmt = "";
      uint8_fmt = "";
      uint4_fmt = "";
      uint_fmt = "";
      break;
    default:
      assert (0);
      break;
    }

  /* And print out the value in the given stream..  */
  if (val == PVM_NULL)
    pk_puts ("null");
  else if (PVM_IS_LONG (val))
    {
      int size = PVM_VAL_LONG_SIZE (val);
      int64_t longval = PVM_VAL_LONG (val);
      uint64_t ulongval;

      pk_term_class ("integer");

      if (size == 64)
        ulongval = (uint64_t) longval;
      else
        ulongval = (uint64_t) longval & ((((uint64_t) 1) << size) - 1);

      if (base == 2)
        pvm_print_binary (ulongval, size, 1);
      else
        {
          if (size == 64)
            pk_printf (long64_fmt, base == 10 ? longval : ulongval);
          else
            pk_printf (long_fmt, PVM_VAL_LONG_SIZE (val),
                       base == 10 ? longval : ulongval);
        }

      pk_term_end_class ("integer");
    }
  else if (PVM_IS_INT (val))
    {
      int size = PVM_VAL_INT_SIZE (val);
      int32_t intval = PVM_VAL_INT (val);
      uint32_t uintval;

      pk_term_class ("integer");

      if (size == 32)
        uintval = (uint32_t) intval;
      else
        uintval = (uint32_t) intval & ((((uint32_t) 1) << size) - 1);

      if (base == 2)
        pvm_print_binary ((uint64_t) uintval, size, 1);
      else
        {
          if (size == 32)
            pk_printf (int32_fmt, base == 10 ? intval : uintval);
          else if (size == 16)
            pk_printf (int16_fmt, base == 10 ? intval : uintval);
          else if (size == 8)
            pk_printf (int8_fmt, base == 10 ? intval : uintval);
          else if (size == 4)
            pk_printf (int4_fmt, base == 10 ? intval : uintval);
          else
            pk_printf (int_fmt, PVM_VAL_INT_SIZE (val),
                       base == 10 ? intval : uintval);
        }

      pk_term_end_class ("integer");
    }
  else if (PVM_IS_ULONG (val))
    {
      int size = PVM_VAL_ULONG_SIZE (val);
      uint64_t ulongval = PVM_VAL_ULONG (val);

      pk_term_class ("integer");

      if (base == 2)
        pvm_print_binary (ulongval, size, 0);
      else
        {
          if (size == 64)
            pk_printf (ulong64_fmt, ulongval);
          else
            pk_printf (ulong_fmt, PVM_VAL_LONG_SIZE (val), ulongval);
        }

      pk_term_end_class ("integer");
    }
  else if (PVM_IS_UINT (val))
    {
      int size = PVM_VAL_UINT_SIZE (val);
      uint32_t uintval = PVM_VAL_UINT (val);

      pk_term_class ("integer");

      if (base == 2)
        pvm_print_binary (uintval, size, 0);
      else
        {
          if (size == 32)
            pk_printf (uint32_fmt, uintval);
          else if (size == 16)
            pk_printf (uint16_fmt, uintval);
          else if (size == 8)
            pk_printf (uint8_fmt, uintval);
          else if (size == 4)
            pk_printf (uint4_fmt, uintval);
          else
            pk_printf (uint_fmt, PVM_VAL_UINT_SIZE (val),
                       uintval);
        }

      pk_term_end_class ("integer");
    }
  else if (PVM_IS_STR (val))
    {
      const char *str = PVM_VAL_STR (val);
      char *str_printable;
      size_t str_size = strlen (PVM_VAL_STR (val));
      size_t printable_size, i, j;

      pk_term_class ("string");

      /* Calculate the length (in bytes) of the printable string
         corresponding to the string value.  */
      for (printable_size = 0, i = 0; i < str_size; i++)
        {
          switch (str[i])
            {
            case '\n': printable_size += 2; break;
            case '\t': printable_size += 2; break;
            case '\\': printable_size += 2; break;
            case '\"': printable_size += 2; break;
            default: printable_size += 1; break;
            }
        }

      /* Now build the printable string.  */
      str_printable = xmalloc (printable_size + 1);
      for (i = 0, j = 0; i < str_size; i++)
        {
          switch (str[i])
            {
            case '\n':
              str_printable[j] = '\\';
              str_printable[j+1] = 'n';
              j += 2;
              break;
            case '\t':
              str_printable[j] = '\\';
              str_printable[j+1] = 't';
              j += 2;
              break;
            case '\\':
              str_printable[j] = '\\';
              str_printable[j+1] = '\\';
              j += 2;
              break;
            case '"':
              str_printable[j] = '\\';
              str_printable[j+1] = '\"';
              j += 2;
              break;
            default:
              str_printable[j] = str[i];
              j++;
              break;
            }
        }
      assert (j == printable_size);
      str_printable[j] = '\0';

      pk_printf ("\"%s\"", str_printable);
      free (str_printable);

      pk_term_end_class ("string");
    }
  else if (PVM_IS_ARR (val))
    {
      size_t nelem, idx;
      pvm_val array_offset = PVM_VAL_ARR_OFFSET (val);

      nelem = PVM_VAL_ULONG (PVM_VAL_ARR_NELEM (val));

      pk_term_class ("array");

      pk_puts ("[");
      for (idx = 0; idx < nelem; idx++)
        {
          pvm_val elem_offset = PVM_VAL_ARR_ELEM_OFFSET (val, idx);
          pvm_val elem_value = PVM_VAL_ARR_ELEM_VALUE (val, idx);

          if (idx != 0)
            pk_puts (",");
          pvm_print_val (elem_value, base, flags);

          if (flags & PVM_PRINT_F_MAPS && elem_offset != PVM_NULL)
            {
              pk_puts ("@");
              pvm_print_val (elem_offset, base, flags);
            }
        }
      pk_puts ("]");

      if (flags & PVM_PRINT_F_MAPS && array_offset != PVM_NULL)
        {
          pk_puts ("@");
          pvm_print_val (array_offset, base, flags);
        }

      pk_term_end_class ("array");
    }
  else if (PVM_IS_SCT (val))
    {
      size_t nelem, idx;
      pvm_val struct_type = PVM_VAL_SCT_TYPE (val);
      pvm_val struct_type_name = PVM_VAL_TYP_S_NAME (struct_type);
      pvm_val pretty_printer = pvm_get_struct_method (val, "_print");

      /* If the struct has a pretty printing method (called _print)
         then use it, unless the PVM is configured to not do so.
         XXX: avoid the global poke_vm here!  */
      if (pvm_pretty_print (poke_vm) && pretty_printer != PVM_NULL)
        {
          pvm_call_pretty_printer (val, pretty_printer);
          return;
        }

      nelem = PVM_VAL_ULONG (PVM_VAL_SCT_NFIELDS (val));

      pk_term_class ("struct");

      if (struct_type_name != PVM_NULL)
        {
          pk_term_class ("struct-type-name");
          pk_puts ( PVM_VAL_STR (struct_type_name));
          pk_term_end_class ("struct-type-name");
        }
      else
        pk_puts ("struct");
      pk_puts (" ");

      pk_printf ("{");
      for (idx = 0; idx < nelem; ++idx)
        {
          pvm_val name = PVM_VAL_SCT_FIELD_NAME(val, idx);
          pvm_val value = PVM_VAL_SCT_FIELD_VALUE(val, idx);
          pvm_val offset = PVM_VAL_SCT_FIELD_OFFSET(val, idx);

          if (idx != 0)
            pk_puts (",");
          if (name != PVM_NULL)
            {
              pk_term_class ("struct-field-name");
              pk_printf ("%s", PVM_VAL_STR (name));
              pk_term_end_class ("struct-field-name");
              pk_puts ("=");
            }
          pvm_print_val (value, base, flags);

          if (flags & PVM_PRINT_F_MAPS && offset != PVM_NULL)
            {
              pk_puts ("@");
              pvm_print_val (offset, base, flags);
            }
        }
      pk_puts ("}");

      pk_term_end_class ("struct");
    }
  else if (PVM_IS_TYP (val))
    {
      pk_term_class ("type");

      switch (PVM_VAL_TYP_CODE (val))
        {
        case PVM_TYPE_INTEGRAL:
          {
            if (!(PVM_VAL_UINT (PVM_VAL_TYP_I_SIGNED (val))))
              pk_puts ("u");

            switch (PVM_VAL_ULONG (PVM_VAL_TYP_I_SIZE (val)))
              {
              case 8: pk_puts ("int8"); break;
              case 16: pk_puts ("int16"); break;
              case 32: pk_puts ("int32"); break;
              case 64: pk_puts ("int64"); break;
              default: assert (0); break;
              }
          }
          break;
        case PVM_TYPE_STRING:
          pk_puts ("string");
          break;
        case PVM_TYPE_ANY:
          pk_term_class ("any");
          pk_puts ("any");
          pk_term_end_class ("any");
          break;
        case PVM_TYPE_ARRAY:
          pvm_print_val (PVM_VAL_TYP_A_ETYPE (val), base, flags);
          pk_puts ("[");
          if (PVM_VAL_TYP_A_BOUND (val) != PVM_NULL)
            pvm_print_val (PVM_VAL_TYP_A_BOUND (val), base, flags);
          pk_puts ("]");
          break;
        case PVM_TYPE_OFFSET:
          pk_puts ("[");
          pvm_print_val (PVM_VAL_TYP_O_BASE_TYPE (val), base, flags);
          pk_puts (" ");
          switch (PVM_VAL_ULONG (PVM_VAL_TYP_O_UNIT (val)))
            {
            case PVM_VAL_OFF_UNIT_BITS:
              pk_puts ("b");
              break;
            case PVM_VAL_OFF_UNIT_BYTES:
              pk_puts ("B");
              break;
            case PVM_VAL_OFF_UNIT_KILOBITS:
              pk_puts ("Kb");
              break;
            case PVM_VAL_OFF_UNIT_KILOBYTES:
              pk_puts ("KB");
              break;
            case PVM_VAL_OFF_UNIT_MEGABITS:
              pk_puts ("Mb");
              break;
            case PVM_VAL_OFF_UNIT_MEGABYTES:
              pk_puts ("MB");
              break;
            case PVM_VAL_OFF_UNIT_GIGABITS:
              pk_puts ("Gb");
              break;
            default:
              assert (0);
            }
          pk_puts ("]");
          break;
        case PVM_TYPE_CLOSURE:
          {
            size_t i, nargs;

            nargs = PVM_VAL_ULONG (PVM_VAL_TYP_C_NARGS (val));

            pk_puts ("(");
            for (i = 0; i < nargs; ++i)
              {
                pvm_val atype = PVM_VAL_TYP_C_ATYPE (val, i);
                pvm_print_val (atype, base, flags);
              }
            pvm_print_val (PVM_VAL_TYP_C_RETURN_TYPE (val), 10, flags);
            break;
          }
        case PVM_TYPE_STRUCT:
          {
            size_t i, nelem;

            nelem = PVM_VAL_ULONG (PVM_VAL_TYP_S_NFIELDS (val));

            pk_puts ("struct {");
            for (i = 0; i < nelem; ++i)
              {
                pvm_val ename = PVM_VAL_TYP_S_FNAME(val, i);
                pvm_val etype = PVM_VAL_TYP_S_FTYPE(val, i);

                if (i != 0)
                  pk_puts (" ");

                pvm_print_val (etype, base, flags);
                if (ename != PVM_NULL)
                  pk_printf (" %s", PVM_VAL_STR (ename));
                pk_puts (";");
              }
            pk_puts ("}");
          break;
          }
        default:
          assert (0);
        }

      pk_term_end_class ("type");
    }
  else if (PVM_IS_OFF (val))
    {
      pk_term_class ("offset");
      pvm_print_val (PVM_VAL_OFF_MAGNITUDE (val), base, flags);
      pk_puts ("#");
      switch (PVM_VAL_ULONG (PVM_VAL_OFF_UNIT (val)))
        {
        case PVM_VAL_OFF_UNIT_BITS:
          pk_puts ("b");
          break;
        case PVM_VAL_OFF_UNIT_NIBBLES:
          pk_puts ("N");
          break;
        case PVM_VAL_OFF_UNIT_BYTES:
          pk_puts ("B");
          break;
        case PVM_VAL_OFF_UNIT_KILOBITS:
          pk_puts ("Kb");
          break;
        case PVM_VAL_OFF_UNIT_KILOBYTES:
          pk_puts ("KB");
          break;
        case PVM_VAL_OFF_UNIT_MEGABITS:
          pk_puts ("Mb");
          break;
        case PVM_VAL_OFF_UNIT_MEGABYTES:
          pk_puts ("MB");
          break;
        case PVM_VAL_OFF_UNIT_GIGABITS:
          pk_puts ("Gb");
          break;
        default:
          /* XXX: print here the name of the base type of the
             offset.  */
          pk_printf ("%" PRIu64, PVM_VAL_ULONG (PVM_VAL_OFF_UNIT (val)));
          break;
        }

      pk_term_end_class ("offset");
    }
  else if (PVM_IS_CLS (val))
    {
      pk_term_class ("special");
      pk_puts ("#<closure>");
      pk_term_end_class ("special");
    }
  else
    assert (0);
}

pvm_val
pvm_typeof (pvm_val val)
{
  pvm_val type;

  if (PVM_IS_INT (val))
    type = pvm_make_integral_type (pvm_make_ulong (PVM_VAL_INT_SIZE (val), 64),
                                   pvm_make_uint (1, 32));
  else if (PVM_IS_UINT (val))
    type = pvm_make_integral_type (pvm_make_ulong (PVM_VAL_UINT_SIZE (val), 64),
                                   pvm_make_uint (0, 32));
  else if (PVM_IS_LONG (val))
    type = pvm_make_integral_type (pvm_make_ulong (PVM_VAL_LONG_SIZE (val), 64),
                                   pvm_make_uint (1, 32));
  else if (PVM_IS_ULONG (val))
    type = pvm_make_integral_type (pvm_make_ulong (PVM_VAL_ULONG_SIZE (val), 64),
                                   pvm_make_uint (0, 32));
  else if (PVM_IS_STR (val))
    type = pvm_make_string_type ();
  else if (PVM_IS_OFF (val))
    type = pvm_make_offset_type (PVM_VAL_OFF_BASE_TYPE (val),
                                 PVM_VAL_OFF_UNIT (val));
  else if (PVM_IS_ARR (val))
    type = PVM_VAL_ARR_TYPE (val);
  else if (PVM_IS_SCT (val))
    type = PVM_VAL_SCT_TYPE (val);
  else
    assert (0);

  return type;
}

int
pvm_type_equal (pvm_val type1, pvm_val type2)
{
  enum pvm_type_code type_code_1 = PVM_VAL_TYP_CODE (type1);
  enum pvm_type_code type_code_2 = PVM_VAL_TYP_CODE (type2);

  if (type_code_1 != type_code_2)
    return 0;

  switch (type_code_1)
    {
    case PVM_TYPE_INTEGRAL:
      {
        size_t t1_size = PVM_VAL_ULONG (PVM_VAL_TYP_I_SIZE (type1));
        size_t t2_size = PVM_VAL_ULONG (PVM_VAL_TYP_I_SIZE (type2));
        uint32_t t1_signed = PVM_VAL_UINT (PVM_VAL_TYP_I_SIGNED (type1));
        uint32_t t2_signed = PVM_VAL_UINT (PVM_VAL_TYP_I_SIGNED (type2));

        return (t1_size == t2_size && t1_signed == t2_signed);
        break;
      }
    case PVM_TYPE_STRING:
    case PVM_TYPE_ANY:
      return 1;
      break;
    case PVM_TYPE_ARRAY:
      return pvm_type_equal (PVM_VAL_TYP_A_ETYPE (type1),
                             PVM_VAL_TYP_A_ETYPE (type2));
      break;
    case PVM_TYPE_STRUCT:
      return (STREQ (PVM_VAL_STR (PVM_VAL_TYP_S_NAME (type1)),
                     PVM_VAL_STR (PVM_VAL_TYP_S_NAME (type2))));
      break;
    case PVM_TYPE_OFFSET:
      return (pvm_type_equal (PVM_VAL_TYP_O_BASE_TYPE (type1),
                              PVM_VAL_TYP_O_BASE_TYPE (type2))
              && (PVM_VAL_ULONG (PVM_VAL_TYP_O_UNIT (type1))
                  == PVM_VAL_ULONG (PVM_VAL_TYP_O_UNIT (type2))));
      break;
    case PVM_TYPE_CLOSURE:
      {
        size_t i, nargs;

        if (PVM_VAL_ULONG (PVM_VAL_TYP_C_NARGS (type1))
            != PVM_VAL_ULONG (PVM_VAL_TYP_C_NARGS (type2)))
          return 0;

        if (!pvm_type_equal (PVM_VAL_TYP_C_RETURN_TYPE (type1),
                             PVM_VAL_TYP_C_RETURN_TYPE (type2)))
          return 0;

        nargs = PVM_VAL_ULONG (PVM_VAL_TYP_C_NARGS (type1));
        for (i = 0; i < nargs; i++)
          {
            if (!pvm_type_equal (PVM_VAL_TYP_C_ATYPE (type1, i),
                                 PVM_VAL_TYP_C_ATYPE (type2, i)))
              return 0;
          }

        return 1;
      }
      break;
    default:
      assert (0);
    }
}

void
pvm_print_string (pvm_val string)
{
  pk_puts (PVM_VAL_STR (string));
}

/* Call a struct pretty-print function in the closure CLS,
   corresponding to the struct VAL.  */

int
pvm_call_pretty_printer (pvm_val val, pvm_val cls)
{
  pvm_routine routine;
  int ret;
  pkl_asm pasm = pkl_asm_new (NULL /* ast */,
                              poke_compiler, 1 /* prologue */);

  /* Remap the struct.  XXX this shouldn't be needed, because it won't
     have any effect in not-mapped structs.  What we need to do is to
     install closures as methods with a duplicated environment.  */
  pkl_asm_insn (pasm, PKL_INSN_PUSH, val);
  pkl_asm_insn (pasm, PKL_INSN_REMAP);
  pkl_asm_insn (pasm, PKL_INSN_DROP);

  /* Call the closure.  */
  pkl_asm_insn (pasm, PKL_INSN_PUSH, cls);
  pkl_asm_insn (pasm, PKL_INSN_CALL);

  /* Run the routine in the poke VM.  */
  routine = pkl_asm_finish (pasm, 1 /* epilogue */, NULL /* pointers */);
  jitter_routine_make_executable_if_needed (routine);
  ret = pvm_run (poke_vm, routine, NULL);
  pvm_destroy_routine (routine);

  return (ret == PVM_EXIT_OK);
}
