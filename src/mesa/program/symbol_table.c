/*
 * Copyright © 2008 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */


#include "main/errors.h"
#include "symbol_table.h"
#include "util/hash_table.h"
#include "util/u_string.h"

struct symbol {
   /** Symbol name. */
   char *name;

    /**
     * Link to the next symbol in the table with the same name
     *
     * The linked list of symbols with the same name is ordered by scope
     * from inner-most to outer-most.
     */
    struct symbol *next_with_same_name;

    /**
     * Link to the next symbol in the table with the same scope
     *
     * The linked list of symbols with the same scope is unordered.  Symbols
     * in this list my have unique names.
     */
    struct symbol *next_with_same_scope;

    /** Scope depth where this symbol was defined. */
    unsigned depth;

    /**
     * Arbitrary user supplied data.
     */
    void *data;
};


/**
 * Element of the scope stack.
 */
struct scope_level {
    /** Link to next (inner) scope level. */
    struct scope_level *next;

    /** Linked list of symbols with the same scope. */
    struct symbol *symbols;
};


/**
 *
 */
struct _mesa_symbol_table {
    /** Hash table containing all symbols in the symbol table. */
    struct hash_table *ht;

    /** Top of scope stack. */
    struct scope_level *current_scope;

    /** Current scope depth. */
    unsigned depth;
};

void
_mesa_symbol_table_pop_scope(struct _mesa_symbol_table *table)
{
    struct scope_level *const scope = table->current_scope;
    struct symbol *sym = scope->symbols;

    table->current_scope = scope->next;
    table->depth--;

    free(scope);

    while (sym != NULL) {
        struct symbol *const next = sym->next_with_same_scope;
        struct hash_entry *hte = _mesa_hash_table_search(table->ht,
                                                         sym->name);
        if (sym->next_with_same_name) {
           /* If there is a symbol with this name in an outer scope update
            * the hash table to point to it.
            */
           hte->data = sym->next_with_same_name;
        } else {
           _mesa_hash_table_remove(table->ht, hte);
        }

        free(sym);
        sym = next;
    }
}


void
_mesa_symbol_table_push_scope(struct _mesa_symbol_table *table)
{
    struct scope_level *const scope = calloc(1, sizeof(*scope));
    if (scope == NULL) {
       _mesa_error_no_memory(__func__);
       return;
    }

    scope->next = table->current_scope;
    table->current_scope = scope;
    table->depth++;
}


static struct symbol *
find_symbol(struct _mesa_symbol_table *table, const char *name)
{
   struct hash_entry *entry = _mesa_hash_table_search(table->ht, name);
   return entry ? (struct symbol *) entry->data : NULL;
}


/**
 * Determine the scope "distance" of a symbol from the current scope
 *
 * \return
 * A non-negative number for the number of scopes between the current scope
 * and the scope where a symbol was defined.  A value of zero means the current
 * scope.  A negative number if the symbol does not exist.
 */
int
_mesa_symbol_table_symbol_scope(struct _mesa_symbol_table *table,
                                const char *name)
{
   struct symbol *const sym = find_symbol(table, name);

   if (sym) {
      assert(sym->depth <= table->depth);
      return table->depth - sym->depth;
   }

   return -1;
}


void *
_mesa_symbol_table_find_symbol(struct _mesa_symbol_table *table,
                               const char *name)
{
   struct symbol *const sym = find_symbol(table, name);
   if (sym)
      return sym->data;

   return NULL;
}


int
_mesa_symbol_table_add_symbol(struct _mesa_symbol_table *table,
                              const char *name, void *declaration)
{
   struct symbol *new_sym;
   uint32_t hash = _mesa_hash_string(name);
   struct hash_entry *entry = _mesa_hash_table_search_pre_hashed(table->ht, hash, name);
   struct symbol *sym = entry ? entry->data : NULL;

   if (sym && sym->depth == table->depth)
      return -1;

   new_sym = calloc(1, sizeof(*sym) + (sym ? 0 : (strlen(name) + 1)));
   if (new_sym == NULL) {
      _mesa_error_no_memory(__func__);
      return -1;
   }

   if (sym) {
      /* Store link to symbol in outer scope with the same name */
      new_sym->next_with_same_name = sym;
      new_sym->name = sym->name;

      entry->data = new_sym;
   } else {
      new_sym->name = (char *)(new_sym + 1);
      strcpy(new_sym->name, name);

      _mesa_hash_table_insert_pre_hashed(table->ht, hash, new_sym->name, new_sym);
   }

   new_sym->next_with_same_scope = table->current_scope->symbols;
   new_sym->data = declaration;
   new_sym->depth = table->depth;

   table->current_scope->symbols = new_sym;

   return 0;
}

int
_mesa_symbol_table_replace_symbol(struct _mesa_symbol_table *table,
                                  const char *name,
                                  void *declaration)
{
    struct symbol *sym = find_symbol(table, name);

    /* If the symbol doesn't exist, it cannot be replaced. */
    if (sym == NULL)
       return -1;

    sym->data = declaration;
    return 0;
}

struct _mesa_symbol_table *
_mesa_symbol_table_ctor(void)
{
    struct _mesa_symbol_table *table = calloc(1, sizeof(*table));

    if (table != NULL) {
       table->ht = _mesa_hash_table_create(NULL, _mesa_hash_string,
                                           _mesa_key_string_equal);

       _mesa_symbol_table_push_scope(table);
    }

    return table;
}


void
_mesa_symbol_table_dtor(struct _mesa_symbol_table *table)
{
   /* Free all the scopes and symbols left in the table.  This is like repeated
    * _mesa_symbol_table_pop_scope(), but not maintining the hash table as we
    * blow it all away.
    */
   while (table->current_scope) {
      struct scope_level *scope = table->current_scope;
      table->current_scope = scope->next;

      while (scope->symbols) {
         struct symbol *sym = scope->symbols;
         scope->symbols = sym->next_with_same_scope;
         free(sym);
      }
      free(scope);
   }

   _mesa_hash_table_destroy(table->ht, NULL);
   free(table);
}
