/*
 * Copyright © 2014 Intel Corporation
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
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Authors:
 *    Connor Abbott (cwabbott0@gmail.com)
 *
 */

#include "float64_glsl.h"
#include "glsl_parser_extras.h"
#include "glsl_to_nir.h"
#include "ir_visitor.h"
#include "ir_hierarchical_visitor.h"
#include "ir.h"
#include "ir_optimization.h"
#include "compiler/nir/nir_control_flow.h"
#include "compiler/nir/nir_builder.h"
#include "compiler/nir/nir_builtin_builder.h"
#include "compiler/nir/nir_deref.h"
#include "main/errors.h"
#include "main/mtypes.h"
#include "main/shaderobj.h"
#include "util/u_math.h"
#include "util/perf/cpu_trace.h"

/*
 * pass to lower GLSL IR to NIR
 *
 * This will lower variable dereferences to loads/stores of corresponding
 * variables in NIR - the variables will be converted to registers in a later
 * pass.
 */

static nir_variable_mode
get_param_mode(ir_variable *param)
{
   switch ((enum ir_variable_mode)(param->data.mode)) {
   case ir_var_const_in:
   case ir_var_function_in:
      return nir_var_function_in;

   case ir_var_function_out:
      return nir_var_function_out;

   case ir_var_function_inout:
      return nir_var_function_inout;

   case ir_var_auto:
   case ir_var_uniform:
   case ir_var_shader_storage:
   case ir_var_temporary:
   default:
      UNREACHABLE("Unsupported function param mode");
   }
}

namespace {

class nir_visitor : public ir_visitor
{
public:
   nir_visitor(nir_shader *shader, const uint8_t *src_blake3);
   nir_visitor(const nir_visitor &) = delete;
   ~nir_visitor();
   nir_visitor & operator=(const nir_visitor &) = delete;

   virtual void visit(ir_variable *);
   virtual void visit(ir_function *);
   virtual void visit(ir_function_signature *);
   virtual void visit(ir_loop *);
   virtual void visit(ir_if *);
   virtual void visit(ir_discard *);
   virtual void visit(ir_demote *);
   virtual void visit(ir_loop_jump *);
   virtual void visit(ir_return *);
   virtual void visit(ir_call *);
   virtual void visit(ir_assignment *);
   virtual void visit(ir_emit_vertex *);
   virtual void visit(ir_end_primitive *);
   virtual void visit(ir_expression *);
   virtual void visit(ir_swizzle *);
   virtual void visit(ir_texture *);
   virtual void visit(ir_constant *);
   virtual void visit(ir_dereference_variable *);
   virtual void visit(ir_dereference_record *);
   virtual void visit(ir_dereference_array *);
   virtual void visit(ir_barrier *);

   void create_function(ir_function_signature *ir);

private:
   void add_instr(nir_instr *instr, unsigned num_components, unsigned bit_size);
   void truncate_after_instruction(ir_exec_node *ir);
   nir_def *evaluate_rvalue(ir_rvalue *ir);

   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_def **srcs);
   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_def *src1);
   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_def *src1,
                       nir_def *src2);
   nir_alu_instr *emit(nir_op op, unsigned dest_size, nir_def *src1,
                       nir_def *src2, nir_def *src3);

   nir_shader *shader;
   nir_function_impl *impl;
   nir_function_impl *global_impl;
   nir_builder b;
   nir_def *result; /* result of the expression tree last visited */

   nir_deref_instr *evaluate_deref(ir_instruction *ir);

   nir_constant *constant_copy(ir_constant *ir, void *mem_ctx);

   /* most recent deref instruction created */
   nir_deref_instr *deref;

   /* whether the IR we're operating on is per-function or global */
   bool is_global;

   ir_function_signature *sig;

   /* map of ir_variable -> nir_variable */
   struct hash_table *var_table;

   /* map of ir_function_signature -> nir_function_overload */
   struct hash_table *overload_table;

   /* set of nir_variable hold sparse result */
   struct set *sparse_variable_set;

   void adjust_sparse_variable(nir_deref_instr *var_deref, const glsl_type *type,
                               nir_def *dest);
};

/*
 * This visitor runs before the main visitor, calling create_function() for
 * each function so that the main visitor can resolve forward references in
 * calls.
 */

class nir_function_visitor : public ir_hierarchical_visitor
{
public:
   nir_function_visitor(nir_visitor *v) : visitor(v)
   {
   }
   virtual ir_visitor_status visit_enter(ir_function *);

private:
   nir_visitor *visitor;
};

} /* end of anonymous namespace */

nir_shader *
glsl_to_nir(struct gl_shader *gl_shader,
            const nir_shader_compiler_options *options,
            const uint8_t *src_blake3)
{
   MESA_TRACE_FUNC();

   nir_shader *shader =
      nir_shader_create(NULL, gl_shader->Stage, options, NULL);

   nir_visitor v1(shader, src_blake3);
   nir_function_visitor v2(&v1);
   v2.run(gl_shader->ir);
   visit_exec_list(gl_shader->ir, &v1);

   /* The GLSL IR won't be needed anymore. */
   ralloc_free(gl_shader->ir);
   gl_shader->ir = NULL;

   nir_validate_shader(shader, "after glsl to nir, before function inline");
   if (should_print_nir(shader)) {
      printf("glsl_to_nir\n");
      nir_print_shader(shader, stdout);
   }

   return shader;
}

nir_visitor::nir_visitor(nir_shader *shader, const uint8_t *src_blake3)
{
   this->shader = shader;
   this->is_global = true;
   this->var_table = _mesa_pointer_hash_table_create(NULL);
   this->overload_table = _mesa_pointer_hash_table_create(NULL);
   this->sparse_variable_set = _mesa_pointer_set_create(NULL);
   this->result = NULL;
   this->impl = NULL;
   this->deref = NULL;
   this->sig = NULL;
   this->global_impl = NULL;
   memset(&this->b, 0, sizeof(this->b));

   if (src_blake3) {
      char blake_as_str[BLAKE3_OUT_LEN * 2 + 1];;
      _mesa_blake3_format(blake_as_str, src_blake3);

      /* Create unique function name of function to temporarily hold global
       * instructions.
       */
      char gloabl_func_name[45];
      snprintf(gloabl_func_name, 45, "%s_%s", "gl_mesa_tmp", blake_as_str);

      nir_function *func = nir_function_create(shader, gloabl_func_name);
      func->is_tmp_globals_wrapper = true;
      this->global_impl = nir_function_impl_create(func);

      this->impl = this->global_impl;
      b = nir_builder_at(nir_after_impl(this->impl));
   }
}

nir_visitor::~nir_visitor()
{
   _mesa_hash_table_destroy(this->var_table, NULL);
   _mesa_hash_table_destroy(this->overload_table, NULL);
   _mesa_set_destroy(this->sparse_variable_set, NULL);
}

nir_deref_instr *
nir_visitor::evaluate_deref(ir_instruction *ir)
{
   ir->accept(this);
   return this->deref;
}

void
nir_visitor::truncate_after_instruction(ir_exec_node *ir)
{
   if (!ir)
      return;

   while (!ir->get_next()->is_tail_sentinel()) {
      ((ir_instruction *)ir->get_next())->remove();
   }
}

nir_constant *
nir_visitor::constant_copy(ir_constant *ir, void *mem_ctx)
{
   if (ir == NULL)
      return NULL;

   nir_constant *ret = rzalloc(mem_ctx, nir_constant);

   const unsigned rows = ir->type->vector_elements;
   const unsigned cols = ir->type->matrix_columns;
   unsigned i;

   ret->num_elements = 0;
   switch (ir->type->base_type) {
   case GLSL_TYPE_UINT:
      /* Only float base types can be matrices. */
      assert(cols == 1);

      for (unsigned r = 0; r < rows; r++)
         ret->values[r].u32 = ir->value.u[r];

      break;

   case GLSL_TYPE_UINT16:
      /* Only float base types can be matrices. */
      assert(cols == 1);

      for (unsigned r = 0; r < rows; r++)
         ret->values[r].u16 = ir->value.u16[r];
      break;

   case GLSL_TYPE_INT:
      /* Only float base types can be matrices. */
      assert(cols == 1);

      for (unsigned r = 0; r < rows; r++)
         ret->values[r].i32 = ir->value.i[r];

      break;

   case GLSL_TYPE_INT16:
      /* Only float base types can be matrices. */
      assert(cols == 1);

      for (unsigned r = 0; r < rows; r++)
         ret->values[r].i16 = ir->value.i16[r];
      break;

   case GLSL_TYPE_FLOAT:
   case GLSL_TYPE_FLOAT16:
   case GLSL_TYPE_DOUBLE:
      if (cols > 1) {
         ret->elements = ralloc_array(mem_ctx, nir_constant *, cols);
         ret->num_elements = cols;
         for (unsigned c = 0; c < cols; c++) {
            nir_constant *col_const = rzalloc(mem_ctx, nir_constant);
            col_const->num_elements = 0;
            switch (ir->type->base_type) {
            case GLSL_TYPE_FLOAT:
               for (unsigned r = 0; r < rows; r++)
                  col_const->values[r].f32 = ir->value.f[c * rows + r];
               break;

            case GLSL_TYPE_FLOAT16:
               for (unsigned r = 0; r < rows; r++)
                  col_const->values[r].u16 = ir->value.f16[c * rows + r];
               break;

            case GLSL_TYPE_DOUBLE:
               for (unsigned r = 0; r < rows; r++)
                  col_const->values[r].f64 = ir->value.d[c * rows + r];
               break;

            default:
               UNREACHABLE("Cannot get here from the first level switch");
            }
            ret->elements[c] = col_const;
         }
      } else {
         switch (ir->type->base_type) {
         case GLSL_TYPE_FLOAT:
            for (unsigned r = 0; r < rows; r++)
               ret->values[r].f32 = ir->value.f[r];
            break;

         case GLSL_TYPE_FLOAT16:
            for (unsigned r = 0; r < rows; r++)
               ret->values[r].u16 = ir->value.f16[r];
            break;

         case GLSL_TYPE_DOUBLE:
            for (unsigned r = 0; r < rows; r++)
               ret->values[r].f64 = ir->value.d[r];
            break;

         default:
            UNREACHABLE("Cannot get here from the first level switch");
         }
      }
      break;

   case GLSL_TYPE_UINT64:
      /* Only float base types can be matrices. */
      assert(cols == 1);

      for (unsigned r = 0; r < rows; r++)
         ret->values[r].u64 = ir->value.u64[r];
      break;

   case GLSL_TYPE_INT64:
      /* Only float base types can be matrices. */
      assert(cols == 1);

      for (unsigned r = 0; r < rows; r++)
         ret->values[r].i64 = ir->value.i64[r];
      break;

   case GLSL_TYPE_BOOL:
      /* Only float base types can be matrices. */
      assert(cols == 1);

      for (unsigned r = 0; r < rows; r++)
         ret->values[r].b = ir->value.b[r];

      break;

   case GLSL_TYPE_STRUCT:
   case GLSL_TYPE_ARRAY:
      ret->elements = ralloc_array(mem_ctx, nir_constant *,
                                   ir->type->length);
      ret->num_elements = ir->type->length;

      for (i = 0; i < ir->type->length; i++)
         ret->elements[i] = constant_copy(ir->const_elements[i], mem_ctx);
      break;

   default:
      UNREACHABLE("not reached");
   }

   return ret;
}

void
nir_visitor::adjust_sparse_variable(nir_deref_instr *var_deref, const glsl_type *type,
                                    nir_def *dest)
{
   const glsl_type *texel_type = glsl_get_field_type(type, "texel");
   assert(texel_type);

   assert(var_deref->deref_type == nir_deref_type_var);
   nir_variable *var = var_deref->var;

   /* Adjust nir_variable type to align with sparse nir instructions.
    * Because the nir_variable is created with struct type from ir_variable,
    * but sparse nir instructions output with vector dest.
    */
   var->type = glsl_simple_type(glsl_get_base_glsl_type(texel_type)->base_type,
                                dest->num_components, 1);

   var_deref->type = var->type;

   /* Record the adjusted variable. */
   _mesa_set_add(this->sparse_variable_set, var);
}

static unsigned
get_nir_how_declared(unsigned how_declared)
{
   if (how_declared == ir_var_hidden)
      return nir_var_hidden;

   if (how_declared == ir_var_declared_implicitly)
      return nir_var_declared_implicitly;

   return nir_var_declared_normally;
}

void
nir_visitor::visit(ir_variable *ir)
{
   /* FINISHME: inout parameters */
   assert(ir->data.mode != ir_var_function_inout);

   if (ir->data.mode == ir_var_function_out)
      return;

   nir_variable *var = rzalloc(shader, nir_variable);
   var->type = ir->type;
   var->name = ralloc_strdup(var, ir->name);

   var->data.assigned = ir->data.assigned;
   var->data.read_only = ir->data.read_only;
   var->data.centroid = ir->data.centroid;
   var->data.sample = ir->data.sample;
   var->data.patch = ir->data.patch;
   var->data.how_declared = get_nir_how_declared(ir->data.how_declared);
   var->data.invariant = ir->data.invariant;
   var->data.explicit_invariant = ir->data.explicit_invariant;
   var->data.location = ir->data.location;
   var->data.must_be_shader_input = ir->data.must_be_shader_input;
   var->data.stream = ir->data.stream;
   if (ir->data.stream & (1u << 31))
      var->data.stream |= NIR_STREAM_PACKED;

   var->data.precision = ir->data.precision;
   var->data.explicit_location = ir->data.explicit_location;
   var->data.matrix_layout = ir->data.matrix_layout;
   var->data.from_named_ifc_block = ir->data.from_named_ifc_block;
   var->data.compact = false;
   var->data.used = ir->data.used;
   var->data.max_array_access = ir->data.max_array_access;
   var->data.implicit_sized_array = ir->data.implicit_sized_array;
   var->data.from_ssbo_unsized_array = ir->data.from_ssbo_unsized_array;

   switch(ir->data.mode) {
   case ir_var_auto:
      if (is_global) {
         var->data.mode = nir_var_shader_temp;
         break;
      }

      FALLTHROUGH;
   case ir_var_temporary:
      var->data.mode = nir_var_function_temp;
      break;

   case ir_var_function_in:
   case ir_var_const_in:
      var->data.mode = nir_var_function_temp;
      break;

   case ir_var_shader_in:
      if (shader->info.stage == MESA_SHADER_GEOMETRY &&
          ir->data.location == VARYING_SLOT_PRIMITIVE_ID) {
         /* For whatever reason, GLSL IR makes gl_PrimitiveIDIn an input */
         var->data.location = SYSTEM_VALUE_PRIMITIVE_ID;
         var->data.mode = nir_var_system_value;
      } else {
         var->data.mode = nir_var_shader_in;
      }
      break;

   case ir_var_shader_out:
      var->data.mode = nir_var_shader_out;
      break;

   case ir_var_uniform:
      if (ir->get_interface_type())
         var->data.mode = nir_var_mem_ubo;
      else if (glsl_type_contains_image(ir->type) && !ir->data.bindless)
         var->data.mode = nir_var_image;
      else
         var->data.mode = nir_var_uniform;
      break;

   case ir_var_shader_storage:
      var->data.mode = nir_var_mem_ssbo;
      break;

   case ir_var_system_value:
      var->data.mode = nir_var_system_value;
      break;

   case ir_var_shader_shared:
      var->data.mode = nir_var_mem_shared;
      break;

   default:
      UNREACHABLE("not reached");
   }

   unsigned mem_access = 0;
   if (ir->data.memory_read_only)
      mem_access |= ACCESS_NON_WRITEABLE;
   if (ir->data.memory_write_only)
      mem_access |= ACCESS_NON_READABLE;
   if (ir->data.memory_coherent)
      mem_access |= ACCESS_COHERENT;
   if (ir->data.memory_volatile)
      mem_access |= ACCESS_VOLATILE;
   if (ir->data.memory_restrict)
      mem_access |= ACCESS_RESTRICT;

   var->interface_type = ir->get_interface_type();

   if (var->data.mode & (nir_var_mem_ubo | nir_var_mem_ssbo)) {
      if (!glsl_type_is_interface(glsl_without_array(ir->type))) {
         /* This variable is one entry in the interface */
         UNUSED bool found = false;
         for (unsigned i = 0; i < ir->get_interface_type()->length; i++) {
            const glsl_struct_field *field =
               &ir->get_interface_type()->fields.structure[i];
            if (strcmp(ir->name, field->name) != 0)
               continue;

            if (field->memory_read_only)
               mem_access |= ACCESS_NON_WRITEABLE;
            if (field->memory_write_only)
               mem_access |= ACCESS_NON_READABLE;
            if (field->memory_coherent)
               mem_access |= ACCESS_COHERENT;
            if (field->memory_volatile)
               mem_access |= ACCESS_VOLATILE;
            if (field->memory_restrict)
               mem_access |= ACCESS_RESTRICT;

            found = true;
            break;
         }
         assert(found);
      }
   }

   var->data.interpolation = ir->data.interpolation;
   var->data.location_frac = ir->data.location_frac;

   switch (ir->data.depth_layout) {
   case ir_depth_layout_none:
      var->data.depth_layout = nir_depth_layout_none;
      break;
   case ir_depth_layout_any:
      var->data.depth_layout = nir_depth_layout_any;
      break;
   case ir_depth_layout_greater:
      var->data.depth_layout = nir_depth_layout_greater;
      break;
   case ir_depth_layout_less:
      var->data.depth_layout = nir_depth_layout_less;
      break;
   case ir_depth_layout_unchanged:
      var->data.depth_layout = nir_depth_layout_unchanged;
      break;
   default:
      UNREACHABLE("not reached");
   }

   var->data.index = ir->data.index;
   var->data.descriptor_set = 0;
   var->data.binding = ir->data.binding;
   var->data.explicit_binding = ir->data.explicit_binding;
   var->data.explicit_offset = ir->data.explicit_xfb_offset;
   var->data.bindless = ir->data.bindless;
   var->data.offset = ir->data.offset;
   var->data.access = (gl_access_qualifier)mem_access;
   var->data.has_initializer = ir->data.has_initializer;
   var->data.is_implicit_initializer = ir->data.is_implicit_initializer;

   if (glsl_type_is_image(glsl_without_array(var->type))) {
      var->data.image.format = ir->data.image_format;
   } else if (var->data.mode == nir_var_shader_out) {
      var->data.xfb.buffer = ir->data.xfb_buffer;
      var->data.xfb.stride = ir->data.xfb_stride;
   }

   var->data.fb_fetch_output = ir->data.fb_fetch_output;
   var->data.explicit_xfb_buffer = ir->data.explicit_xfb_buffer;
   var->data.explicit_xfb_stride = ir->data.explicit_xfb_stride;

   if (ir->is_interface_instance()) {
      int *max_ifc_array_access = ir->get_max_ifc_array_access();
      if (max_ifc_array_access) {
         var->max_ifc_array_access =
            rzalloc_array(var, int, ir->get_interface_type()->length);
         memcpy(var->max_ifc_array_access, max_ifc_array_access,
                ir->get_interface_type()->length * sizeof(unsigned));
      }
   }

   var->num_state_slots = ir->get_num_state_slots();
   if (var->num_state_slots > 0) {
      var->state_slots = rzalloc_array(var, nir_state_slot,
                                       var->num_state_slots);

      ir_state_slot *state_slots = ir->get_state_slots();
      for (unsigned i = 0; i < var->num_state_slots; i++) {
         for (unsigned j = 0; j < 4; j++)
            var->state_slots[i].tokens[j] = state_slots[i].tokens[j];
      }
   } else {
      var->state_slots = NULL;
   }

   /* Values declared const will have ir->constant_value instead of
    * ir->constant_initializer.
    */
   if (ir->constant_initializer)
      var->constant_initializer = constant_copy(ir->constant_initializer, var);
   else
      var->constant_initializer = constant_copy(ir->constant_value, var);

   if (var->data.mode == nir_var_function_temp)
      nir_function_impl_add_variable(impl, var);
   else
      nir_shader_add_variable(shader, var);

   _mesa_hash_table_insert(var_table, ir, var);
}

ir_visitor_status
nir_function_visitor::visit_enter(ir_function *ir)
{
   ir_foreach_in_list(ir_function_signature, sig, &ir->signatures) {
      visitor->create_function(sig);
   }
   return visit_continue_with_parent;
}

void
nir_visitor::create_function(ir_function_signature *ir)
{
   if (ir->is_intrinsic())
      return;

   nir_function *func = nir_function_create(shader, ir->function_name());
   if (strcmp(ir->function_name(), "main") == 0)
      func->is_entrypoint = true;

   func->num_params = ir->parameters.length() +
                      (ir->return_type != &glsl_type_builtin_void);
   func->params = rzalloc_array(shader, nir_parameter, func->num_params);

   unsigned np = 0;
   if (ir->return_type != &glsl_type_builtin_void) {
      /* The return value is a variable deref (basically an out parameter) */
      func->params[np].num_components = 1;
      func->params[np].bit_size = 32;
      func->params[np].type = ir->return_type;
      func->params[np].is_return = true;
      func->params[np].mode = nir_var_function_out;
      np++;
   }

   ir_foreach_in_list(ir_variable, param, &ir->parameters) {
      func->params[np].num_components = 1;
      func->params[np].bit_size = 32;

      func->params[np].type = param->type;
      func->params[np].is_return = false;
      func->params[np].mode = get_param_mode(param);
      func->params[np].implicit_conversion_prohibited =
         !!param->data.implicit_conversion_prohibited;
      np++;
   }
   assert(np == func->num_params);

   func->is_subroutine = ir->function()->is_subroutine;
   func->num_subroutine_types = ir->function()->num_subroutine_types;
   func->subroutine_index = ir->function()->subroutine_index;
   func->subroutine_types =
      ralloc_array(func, const struct glsl_type *, func->num_subroutine_types);
   for (int i = 0; i < func->num_subroutine_types; i++)
     func->subroutine_types[i] = ir->function()->subroutine_types[i];

   _mesa_hash_table_insert(this->overload_table, ir, func);
}

void
nir_visitor::visit(ir_function *ir)
{
   ir_foreach_in_list(ir_function_signature, sig, &ir->signatures)
      sig->accept(this);
}

void
nir_visitor::visit(ir_function_signature *ir)
{
   if (ir->is_intrinsic())
      return;

   this->sig = ir;

   struct hash_entry *entry =
      _mesa_hash_table_search(this->overload_table, ir);

   assert(entry);
   nir_function *func = (nir_function *) entry->data;

   if (ir->is_defined) {
      nir_function_impl *impl = nir_function_impl_create(func);
      this->impl = impl;

      this->is_global = false;

      b = nir_builder_at(nir_after_impl(impl));

      visit_exec_list(&ir->body, this);

      this->impl = global_impl;
      if (this->impl)
         b = nir_builder_at(nir_after_impl(this->impl));

      this->is_global = true;
   }
}

void
nir_visitor::visit(ir_loop *ir)
{
   nir_push_loop(&b);
   visit_exec_list(&ir->body_instructions, this);
   nir_pop_loop(&b, NULL);
}

void
nir_visitor::visit(ir_if *ir)
{
   nir_push_if(&b, evaluate_rvalue(ir->condition));
   visit_exec_list(&ir->then_instructions, this);
   nir_push_else(&b, NULL);
   visit_exec_list(&ir->else_instructions, this);
   nir_pop_if(&b, NULL);
}

void
nir_visitor::visit(ir_discard *ir)
{
   /*
    * discards aren't treated as control flow, because before we lower them
    * they can appear anywhere in the shader and the stuff after them may still
    * be executed (yay, crazy GLSL rules!). However, after lowering, all the
    * discards will be immediately followed by a return.
    */

   if (ir->condition)
      nir_discard_if(&b, evaluate_rvalue(ir->condition));
   else
      nir_discard(&b);
}

void
nir_visitor::visit(ir_demote *ir)
{
   nir_demote(&b);
}

void
nir_visitor::visit(ir_emit_vertex *ir)
{
   nir_emit_vertex(&b, (unsigned)ir->stream_id());
}

void
nir_visitor::visit(ir_end_primitive *ir)
{
   nir_end_primitive(&b, (unsigned)ir->stream_id());
}

void
nir_visitor::visit(ir_loop_jump *ir)
{
   nir_jump_type type;
   switch (ir->mode) {
   case ir_loop_jump::jump_break:
      type = nir_jump_break;
      break;
   case ir_loop_jump::jump_continue:
      type = nir_jump_continue;
      break;
   default:
      UNREACHABLE("not reached");
   }

   nir_jump_instr *instr = nir_jump_instr_create(this->shader, type);
   nir_builder_instr_insert(&b, &instr->instr);

   /* Eliminate all instructions after the jump, since they are unreachable
    * and NIR considers adding these instructions illegal.
    */
   truncate_after_instruction(ir);
}

void
nir_visitor::visit(ir_return *ir)
{
   if (ir->value != NULL) {
      nir_deref_instr *ret_deref =
         nir_build_deref_cast(&b, nir_load_param(&b, 0),
                              nir_var_function_temp, ir->value->type, 0);

      if (glsl_type_is_vector_or_scalar(ir->value->type)) {
         nir_store_deref(&b, ret_deref, evaluate_rvalue(ir->value), ~0);
      } else {
         nir_copy_deref(&b, ret_deref, evaluate_deref(ir->value));
      }
   }

   nir_jump_instr *instr = nir_jump_instr_create(this->shader, nir_jump_return);
   nir_builder_instr_insert(&b, &instr->instr);

   /* Eliminate all instructions after the jump, since they are unreachable
    * and NIR considers adding these instructions illegal.
    */
   truncate_after_instruction(ir);
}

static void
intrinsic_set_std430_align(nir_intrinsic_instr *intrin, const glsl_type *type)
{
   unsigned bit_size = glsl_type_is_boolean(type) ? 32 : glsl_get_bit_size(type);
   unsigned pow2_components = util_next_power_of_two(type->vector_elements);
   nir_intrinsic_set_align(intrin, (bit_size / 8) * pow2_components, 0);
}

/* Accumulate any qualifiers along the deref chain to get the actual
 * load/store qualifier.
 */

static enum gl_access_qualifier
deref_get_qualifier(nir_deref_instr *deref)
{
   nir_deref_path path;
   nir_deref_path_init(&path, deref, NULL);

   /* Function params can lead to a deref cast just return zero as these
    * params have no qualifers anyway.
    */
   if (path.path[0]->deref_type != nir_deref_type_var)
      return (gl_access_qualifier) 0;

   unsigned qualifiers = path.path[0]->var->data.access;

   const glsl_type *parent_type = path.path[0]->type;
   for (nir_deref_instr **cur_ptr = &path.path[1]; *cur_ptr; cur_ptr++) {
      nir_deref_instr *cur = *cur_ptr;

      if (glsl_type_is_interface(parent_type)) {
         const struct glsl_struct_field *field =
            &parent_type->fields.structure[cur->strct.index];
         if (field->memory_read_only)
            qualifiers |= ACCESS_NON_WRITEABLE;
         if (field->memory_write_only)
            qualifiers |= ACCESS_NON_READABLE;
         if (field->memory_coherent)
            qualifiers |= ACCESS_COHERENT;
         if (field->memory_volatile)
            qualifiers |= ACCESS_VOLATILE;
         if (field->memory_restrict)
            qualifiers |= ACCESS_RESTRICT;
      }

      parent_type = cur->type;
   }

   nir_deref_path_finish(&path);

   return (gl_access_qualifier) qualifiers;
}

static nir_op
get_reduction_op(enum ir_intrinsic_id id, const glsl_type *type)
{
#define IR_CASE(op) \
   case ir_intrinsic_reduce_##op: \
   case ir_intrinsic_inclusive_##op: \
   case ir_intrinsic_exclusive_##op: \
   case ir_intrinsic_clustered_##op: \
      return CONV_OP(op);

   switch (id) {

#define CONV_OP(op) \
      type->base_type == GLSL_TYPE_INT || type->base_type == GLSL_TYPE_UINT ? \
         nir_op_i##op : nir_op_f##op

   IR_CASE(add)
   IR_CASE(mul)

#undef CONV_OP
#define CONV_OP(op) \
      type->base_type == GLSL_TYPE_INT ? nir_op_i##op : \
         (type->base_type == GLSL_TYPE_UINT ? nir_op_u##op : nir_op_f##op)

   IR_CASE(min)
   IR_CASE(max)

#undef CONV_OP
#define CONV_OP(op) nir_op_i##op

   IR_CASE(and)
   IR_CASE(or)
   IR_CASE(xor)

#undef CONV_OP

   default:
      UNREACHABLE("not reached");
   }

#undef IR_CASE
}

void
nir_visitor::visit(ir_call *ir)
{
   if (ir->callee->is_intrinsic()) {
      nir_intrinsic_op op;

      /* Initialize to something because gcc complains otherwise */
      nir_atomic_op atomic_op = nir_atomic_op_iadd;

      switch (ir->callee->intrinsic_id) {
      case ir_intrinsic_generic_atomic_add:
         op = nir_intrinsic_deref_atomic;
         atomic_op = glsl_type_is_integer_32_64(ir->return_deref->type)
            ? nir_atomic_op_iadd : nir_atomic_op_fadd;
         break;
      case ir_intrinsic_generic_atomic_and:
         op = nir_intrinsic_deref_atomic;
         atomic_op = nir_atomic_op_iand;
         break;
      case ir_intrinsic_generic_atomic_or:
         op = nir_intrinsic_deref_atomic;
         atomic_op = nir_atomic_op_ior;
         break;
      case ir_intrinsic_generic_atomic_xor:
         op = nir_intrinsic_deref_atomic;
         atomic_op = nir_atomic_op_ixor;
         break;
      case ir_intrinsic_generic_atomic_min:
         assert(ir->return_deref);
         op = nir_intrinsic_deref_atomic;
         if (ir->return_deref->type == &glsl_type_builtin_int ||
             ir->return_deref->type == &glsl_type_builtin_int64_t)
             atomic_op = nir_atomic_op_imin;
         else if (ir->return_deref->type == &glsl_type_builtin_uint ||
                  ir->return_deref->type == &glsl_type_builtin_uint64_t)
             atomic_op = nir_atomic_op_umin;
         else if (ir->return_deref->type == &glsl_type_builtin_float)
             atomic_op = nir_atomic_op_fmin;
         else
            UNREACHABLE("Invalid type");
         break;
      case ir_intrinsic_generic_atomic_max:
         assert(ir->return_deref);
         op = nir_intrinsic_deref_atomic;
         if (ir->return_deref->type == &glsl_type_builtin_int ||
             ir->return_deref->type == &glsl_type_builtin_int64_t)
             atomic_op = nir_atomic_op_imax;
         else if (ir->return_deref->type == &glsl_type_builtin_uint ||
                  ir->return_deref->type == &glsl_type_builtin_uint64_t)
             atomic_op = nir_atomic_op_umax;
         else if (ir->return_deref->type == &glsl_type_builtin_float)
             atomic_op = nir_atomic_op_fmax;
         else
            UNREACHABLE("Invalid type");
         break;
      case ir_intrinsic_generic_atomic_exchange:
         op = nir_intrinsic_deref_atomic;
         atomic_op = nir_atomic_op_xchg;
         break;
      case ir_intrinsic_generic_atomic_comp_swap:
         op = nir_intrinsic_deref_atomic_swap;
         atomic_op = glsl_type_is_integer_32_64(ir->return_deref->type)
            ? nir_atomic_op_cmpxchg
            : nir_atomic_op_fcmpxchg;
         break;
      case ir_intrinsic_atomic_counter_read:
         op = nir_intrinsic_atomic_counter_read_deref;
         break;
      case ir_intrinsic_atomic_counter_increment:
         op = nir_intrinsic_atomic_counter_inc_deref;
         break;
      case ir_intrinsic_atomic_counter_predecrement:
         op = nir_intrinsic_atomic_counter_pre_dec_deref;
         break;
      case ir_intrinsic_atomic_counter_add:
         op = nir_intrinsic_atomic_counter_add_deref;
         break;
      case ir_intrinsic_atomic_counter_and:
         op = nir_intrinsic_atomic_counter_and_deref;
         break;
      case ir_intrinsic_atomic_counter_or:
         op = nir_intrinsic_atomic_counter_or_deref;
         break;
      case ir_intrinsic_atomic_counter_xor:
         op = nir_intrinsic_atomic_counter_xor_deref;
         break;
      case ir_intrinsic_atomic_counter_min:
         op = nir_intrinsic_atomic_counter_min_deref;
         break;
      case ir_intrinsic_atomic_counter_max:
         op = nir_intrinsic_atomic_counter_max_deref;
         break;
      case ir_intrinsic_atomic_counter_exchange:
         op = nir_intrinsic_atomic_counter_exchange_deref;
         break;
      case ir_intrinsic_atomic_counter_comp_swap:
         op = nir_intrinsic_atomic_counter_comp_swap_deref;
         break;
      case ir_intrinsic_image_load:
         op = nir_intrinsic_image_deref_load;
         break;
      case ir_intrinsic_image_store:
         op = nir_intrinsic_image_deref_store;
         break;
      case ir_intrinsic_image_atomic_add:
         op = nir_intrinsic_image_deref_atomic;
         atomic_op = glsl_type_is_integer_32_64(ir->return_deref->type)
            ? nir_atomic_op_iadd
            : nir_atomic_op_fadd;
         break;
      case ir_intrinsic_image_atomic_min:
         op = nir_intrinsic_image_deref_atomic;
         if (ir->return_deref->type == &glsl_type_builtin_int)
            atomic_op = nir_atomic_op_imin;
         else if (ir->return_deref->type == &glsl_type_builtin_uint)
            atomic_op = nir_atomic_op_umin;
         else
            UNREACHABLE("Invalid type");
         break;
      case ir_intrinsic_image_atomic_max:
         op = nir_intrinsic_image_deref_atomic;
         if (ir->return_deref->type == &glsl_type_builtin_int)
            atomic_op = nir_atomic_op_imax;
         else if (ir->return_deref->type == &glsl_type_builtin_uint)
            atomic_op = nir_atomic_op_umax;
         else
            UNREACHABLE("Invalid type");
         break;
      case ir_intrinsic_image_atomic_and:
         op = nir_intrinsic_image_deref_atomic;
         atomic_op = nir_atomic_op_iand;
         break;
      case ir_intrinsic_image_atomic_or:
         op = nir_intrinsic_image_deref_atomic;
         atomic_op = nir_atomic_op_ior;
         break;
      case ir_intrinsic_image_atomic_xor:
         op = nir_intrinsic_image_deref_atomic;
         atomic_op = nir_atomic_op_ixor;
         break;
      case ir_intrinsic_image_atomic_exchange:
         op = nir_intrinsic_image_deref_atomic;
         atomic_op = nir_atomic_op_xchg;
         break;
      case ir_intrinsic_image_atomic_comp_swap:
         op = nir_intrinsic_image_deref_atomic_swap;
         atomic_op = nir_atomic_op_cmpxchg;
         break;
      case ir_intrinsic_image_atomic_inc_wrap:
         op = nir_intrinsic_image_deref_atomic;
         atomic_op = nir_atomic_op_inc_wrap;
         break;
      case ir_intrinsic_image_atomic_dec_wrap:
         op = nir_intrinsic_image_deref_atomic;
         atomic_op = nir_atomic_op_dec_wrap;
         break;
      case ir_intrinsic_memory_barrier:
      case ir_intrinsic_memory_barrier_buffer:
      case ir_intrinsic_memory_barrier_image:
      case ir_intrinsic_memory_barrier_shared:
      case ir_intrinsic_memory_barrier_atomic_counter:
      case ir_intrinsic_group_memory_barrier:
      case ir_intrinsic_subgroup_barrier:
      case ir_intrinsic_subgroup_memory_barrier:
      case ir_intrinsic_subgroup_memory_barrier_buffer:
      case ir_intrinsic_subgroup_memory_barrier_shared:
      case ir_intrinsic_subgroup_memory_barrier_image:
         op = nir_intrinsic_barrier;
         break;
      case ir_intrinsic_image_size:
         op = nir_intrinsic_image_deref_size;
         break;
      case ir_intrinsic_image_samples:
         op = nir_intrinsic_image_deref_samples;
         break;
      case ir_intrinsic_image_sparse_load:
         op = nir_intrinsic_image_deref_sparse_load;
         break;
      case ir_intrinsic_shader_clock:
      case ir_intrinsic_shader_clock_realtime:
         op = nir_intrinsic_shader_clock;
         break;
      case ir_intrinsic_begin_invocation_interlock:
         op = nir_intrinsic_begin_invocation_interlock;
         break;
      case ir_intrinsic_end_invocation_interlock:
         op = nir_intrinsic_end_invocation_interlock;
         break;
      case ir_intrinsic_vote_any:
         op = nir_intrinsic_vote_any;
         break;
      case ir_intrinsic_vote_all:
         op = nir_intrinsic_vote_all;
         break;
      case ir_intrinsic_vote_eq: {
         ir_rvalue *rvalue = (ir_rvalue *) ir->actual_parameters.get_head();
         op = glsl_type_is_integer(rvalue->type) ? nir_intrinsic_vote_ieq : nir_intrinsic_vote_feq;
         break;
      }
      case ir_intrinsic_ballot:
         op = nir_intrinsic_ballot;
         break;
      case ir_intrinsic_read_invocation:
         op = nir_intrinsic_read_invocation;
         break;
      case ir_intrinsic_read_first_invocation:
         op = nir_intrinsic_read_first_invocation;
         break;
      case ir_intrinsic_helper_invocation:
         op = nir_intrinsic_is_helper_invocation;
         break;
      case ir_intrinsic_is_sparse_texels_resident:
         op = nir_intrinsic_is_sparse_texels_resident;
         break;
      case ir_intrinsic_elect:
         op = nir_intrinsic_elect;
         break;
      case ir_intrinsic_inverse_ballot:
         op = nir_intrinsic_inverse_ballot;
         break;
      case ir_intrinsic_ballot_bit_extract:
         op = nir_intrinsic_ballot_bitfield_extract;
         break;
      case ir_intrinsic_ballot_bit_count:
         op = nir_intrinsic_ballot_bit_count_reduce;
         break;
      case ir_intrinsic_ballot_inclusive_bit_count:
         op = nir_intrinsic_ballot_bit_count_inclusive;
         break;
      case ir_intrinsic_ballot_exclusive_bit_count:
         op = nir_intrinsic_ballot_bit_count_exclusive;
         break;
      case ir_intrinsic_ballot_find_lsb:
         op = nir_intrinsic_ballot_find_lsb;
         break;
      case ir_intrinsic_ballot_find_msb:
         op = nir_intrinsic_ballot_find_msb;
         break;
      case ir_intrinsic_shuffle:
         op = nir_intrinsic_shuffle;
         break;
      case ir_intrinsic_shuffle_xor:
         op = nir_intrinsic_shuffle_xor;
         break;
      case ir_intrinsic_shuffle_up:
         op = nir_intrinsic_shuffle_up;
         break;
      case ir_intrinsic_shuffle_down:
         op = nir_intrinsic_shuffle_down;
         break;
      case ir_intrinsic_reduce_add:
      case ir_intrinsic_reduce_mul:
      case ir_intrinsic_reduce_min:
      case ir_intrinsic_reduce_max:
      case ir_intrinsic_reduce_and:
      case ir_intrinsic_reduce_or:
      case ir_intrinsic_reduce_xor:
      case ir_intrinsic_clustered_add:
      case ir_intrinsic_clustered_mul:
      case ir_intrinsic_clustered_min:
      case ir_intrinsic_clustered_max:
      case ir_intrinsic_clustered_and:
      case ir_intrinsic_clustered_or:
      case ir_intrinsic_clustered_xor:
         op = nir_intrinsic_reduce;
         break;
      case ir_intrinsic_inclusive_add:
      case ir_intrinsic_inclusive_mul:
      case ir_intrinsic_inclusive_min:
      case ir_intrinsic_inclusive_max:
      case ir_intrinsic_inclusive_and:
      case ir_intrinsic_inclusive_or:
      case ir_intrinsic_inclusive_xor:
         op = nir_intrinsic_inclusive_scan;
         break;
      case ir_intrinsic_exclusive_add:
      case ir_intrinsic_exclusive_mul:
      case ir_intrinsic_exclusive_min:
      case ir_intrinsic_exclusive_max:
      case ir_intrinsic_exclusive_and:
      case ir_intrinsic_exclusive_or:
      case ir_intrinsic_exclusive_xor:
         op = nir_intrinsic_exclusive_scan;
         break;
      case ir_intrinsic_quad_broadcast:
         op = nir_intrinsic_quad_broadcast;
         break;
      case ir_intrinsic_quad_swap_horizontal:
         op = nir_intrinsic_quad_swap_horizontal;
         break;
      case ir_intrinsic_quad_swap_vertical:
         op = nir_intrinsic_quad_swap_vertical;
         break;
      case ir_intrinsic_quad_swap_diagonal:
         op = nir_intrinsic_quad_swap_diagonal;
         break;
      default:
         UNREACHABLE("not reached");
      }

      nir_intrinsic_instr *instr = nir_intrinsic_instr_create(shader, op);
      nir_def *ret = &instr->def;

      switch (op) {
      case nir_intrinsic_deref_atomic:
      case nir_intrinsic_deref_atomic_swap: {
         int param_count = ir->actual_parameters.length();
         assert(param_count == 2 || param_count == 3);

         /* Deref */
         ir_exec_node *param = ir->actual_parameters.get_head();
         ir_rvalue *rvalue = (ir_rvalue *) param;
         ir_dereference *deref = rvalue->as_dereference();
         ir_swizzle *swizzle = NULL;
         if (!deref) {
            /* We may have a swizzle to pick off a single vec4 component */
            swizzle = rvalue->as_swizzle();
            assert(swizzle && swizzle->type->vector_elements == 1);
            deref = swizzle->val->as_dereference();
            assert(deref);
         }
         nir_deref_instr *nir_deref = evaluate_deref(deref);
         if (swizzle) {
            nir_deref = nir_build_deref_array_imm(&b, nir_deref,
                                                  swizzle->mask.x);
         }
         instr->src[0] = nir_src_for_ssa(&nir_deref->def);

         nir_intrinsic_set_atomic_op(instr, atomic_op);
         nir_intrinsic_set_access(instr, deref_get_qualifier(nir_deref));

         /* data1 parameter (this is always present) */
         param = param->get_next();
         ir_instruction *inst = (ir_instruction *) param;
         instr->src[1] = nir_src_for_ssa(evaluate_rvalue(inst->as_rvalue()));

         /* data2 parameter (only with atomic_comp_swap) */
         if (param_count == 3) {
            assert(op == nir_intrinsic_deref_atomic_swap);
            param = param->get_next();
            inst = (ir_instruction *) param;
            instr->src[2] = nir_src_for_ssa(evaluate_rvalue(inst->as_rvalue()));
         }

         /* Atomic result */
         assert(ir->return_deref);
         if (glsl_type_is_integer_64(ir->return_deref->type)) {
            nir_def_init(&instr->instr, &instr->def,
                         ir->return_deref->type->vector_elements, 64);
         } else {
            nir_def_init(&instr->instr, &instr->def,
                         ir->return_deref->type->vector_elements, 32);
         }
         nir_builder_instr_insert(&b, &instr->instr);
         break;
      }
      case nir_intrinsic_atomic_counter_read_deref:
      case nir_intrinsic_atomic_counter_inc_deref:
      case nir_intrinsic_atomic_counter_pre_dec_deref:
      case nir_intrinsic_atomic_counter_add_deref:
      case nir_intrinsic_atomic_counter_min_deref:
      case nir_intrinsic_atomic_counter_max_deref:
      case nir_intrinsic_atomic_counter_and_deref:
      case nir_intrinsic_atomic_counter_or_deref:
      case nir_intrinsic_atomic_counter_xor_deref:
      case nir_intrinsic_atomic_counter_exchange_deref:
      case nir_intrinsic_atomic_counter_comp_swap_deref: {
         /* Set the counter variable dereference. */
         ir_exec_node *param = ir->actual_parameters.get_head();
         ir_dereference *counter = (ir_dereference *)param;

         instr->src[0] = nir_src_for_ssa(&evaluate_deref(counter)->def);
         param = param->get_next();

         /* Set the intrinsic destination. */
         if (ir->return_deref) {
            nir_def_init(&instr->instr, &instr->def, 1, 32);
         }

         /* Set the intrinsic parameters. */
         if (!param->is_tail_sentinel()) {
            instr->src[1] =
               nir_src_for_ssa(evaluate_rvalue((ir_rvalue *)param));
            param = param->get_next();
         }

         if (!param->is_tail_sentinel()) {
            instr->src[2] =
               nir_src_for_ssa(evaluate_rvalue((ir_dereference *)param));
            param = param->get_next();
         }

         nir_builder_instr_insert(&b, &instr->instr);
         break;
      }
      case nir_intrinsic_image_deref_load:
      case nir_intrinsic_image_deref_store:
      case nir_intrinsic_image_deref_atomic:
      case nir_intrinsic_image_deref_atomic_swap:
      case nir_intrinsic_image_deref_samples:
      case nir_intrinsic_image_deref_size:
      case nir_intrinsic_image_deref_sparse_load: {
         /* Set the image variable dereference. */
         ir_exec_node *param = ir->actual_parameters.get_head();
         ir_dereference *image = (ir_dereference *)param;
         nir_deref_instr *deref = evaluate_deref(image);
         const glsl_type *type = deref->type;

         nir_intrinsic_set_access(instr, deref_get_qualifier(deref));

         if (op == nir_intrinsic_image_deref_atomic ||
             op == nir_intrinsic_image_deref_atomic_swap) {
            nir_intrinsic_set_atomic_op(instr, atomic_op);
         }

         instr->src[0] = nir_src_for_ssa(&deref->def);
         param = param->get_next();
         nir_intrinsic_set_image_dim(instr,
            (glsl_sampler_dim)type->sampler_dimensionality);
         nir_intrinsic_set_image_array(instr, type->sampler_array);

         /* Set the intrinsic destination. */
         if (ir->return_deref) {
            unsigned num_components;
            if (op == nir_intrinsic_image_deref_sparse_load) {
               const glsl_type *dest_type =
                  glsl_get_field_type(ir->return_deref->type, "texel");
               /* One extra component to hold residency code. */
               num_components = dest_type->vector_elements + 1;
            } else
               num_components = ir->return_deref->type->vector_elements;

            nir_def_init(&instr->instr, &instr->def, num_components, 32);
         }

         if (op == nir_intrinsic_image_deref_size) {
            instr->num_components = instr->def.num_components;
         } else if (op == nir_intrinsic_image_deref_load ||
                    op == nir_intrinsic_image_deref_sparse_load) {
            instr->num_components = instr->def.num_components;
            nir_intrinsic_set_dest_type(instr,
               nir_get_nir_type_for_glsl_base_type(type->sampled_type));
         } else if (op == nir_intrinsic_image_deref_store) {
            instr->num_components = 4;
            nir_intrinsic_set_src_type(instr,
               nir_get_nir_type_for_glsl_base_type(type->sampled_type));
         }

         if (op == nir_intrinsic_image_deref_size ||
             op == nir_intrinsic_image_deref_samples) {
            /* image_deref_size takes an LOD parameter which is always 0
             * coming from GLSL.
             */
            if (op == nir_intrinsic_image_deref_size)
               instr->src[1] = nir_src_for_ssa(nir_imm_int(&b, 0));
            nir_builder_instr_insert(&b, &instr->instr);
            break;
         }

         /* Set the address argument, extending the coordinate vector to four
          * components.
          */
         nir_def *src_addr =
            evaluate_rvalue((ir_rvalue *)param);
         nir_def *srcs[4];

         for (int i = 0; i < 4; i++) {
            if (i < glsl_get_sampler_coordinate_components(type))
               srcs[i] = nir_channel(&b, src_addr, i);
            else
               srcs[i] = nir_undef(&b, 1, 32);
         }

         instr->src[1] = nir_src_for_ssa(nir_vec(&b, srcs, 4));
         param = param->get_next();

         /* Set the sample argument, which is undefined for single-sample
          * images.
          */
         if (type->sampler_dimensionality == GLSL_SAMPLER_DIM_MS) {
            instr->src[2] =
               nir_src_for_ssa(evaluate_rvalue((ir_dereference *)param));
            param = param->get_next();
         } else {
            instr->src[2] = nir_src_for_ssa(nir_undef(&b, 1, 32));
         }

         /* Set the intrinsic parameters. */
         if (!param->is_tail_sentinel()) {
            instr->src[3] =
               nir_src_for_ssa(evaluate_rvalue((ir_rvalue *)param));
            param = param->get_next();
         } else if (op == nir_intrinsic_image_deref_load ||
                    op == nir_intrinsic_image_deref_sparse_load) {
            instr->src[3] = nir_src_for_ssa(nir_imm_int(&b, 0)); /* LOD */
         }

         if (!param->is_tail_sentinel()) {
            instr->src[4] =
               nir_src_for_ssa(evaluate_rvalue((ir_rvalue *)param));
            param = param->get_next();
         } else if (op == nir_intrinsic_image_deref_store) {
            instr->src[4] = nir_src_for_ssa(nir_imm_int(&b, 0)); /* LOD */
         }

         nir_builder_instr_insert(&b, &instr->instr);
         break;
      }
      case nir_intrinsic_barrier: {
         /* The nir_intrinsic_barrier follows the general
          * semantics of SPIR-V memory barriers, so this and other memory
          * barriers use the mapping based on GLSL->SPIR-V from
          *
          *   https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_gl_spirv.txt
          */
         if (ir->callee->intrinsic_id == ir_intrinsic_subgroup_barrier) {
            nir_barrier(&b, SCOPE_SUBGROUP, SCOPE_SUBGROUP, NIR_MEMORY_ACQ_REL,
                        nir_var_image | nir_var_mem_ssbo | nir_var_mem_shared | nir_var_mem_global);
            break;
         }

         mesa_scope scope;
         unsigned modes;
         switch (ir->callee->intrinsic_id) {
         case ir_intrinsic_memory_barrier:
            scope = SCOPE_DEVICE;
            modes = nir_var_image |
                    nir_var_mem_ssbo |
                    nir_var_mem_shared |
                    nir_var_mem_global;
            break;
         case ir_intrinsic_memory_barrier_buffer:
            scope = SCOPE_DEVICE;
            modes = nir_var_mem_ssbo |
                    nir_var_mem_global;
            break;
         case ir_intrinsic_memory_barrier_image:
            scope = SCOPE_DEVICE;
            modes = nir_var_image;
            break;
         case ir_intrinsic_memory_barrier_shared:
            /* Both ARB_gl_spirv and glslang lower this to Device scope, so
             * follow their lead.  Note GL_KHR_vulkan_glsl also does
             * something similar.
             */
            scope = SCOPE_DEVICE;
            modes = nir_var_mem_shared;
            break;
         case ir_intrinsic_group_memory_barrier:
            scope = SCOPE_WORKGROUP;
            modes = nir_var_image |
                    nir_var_mem_ssbo |
                    nir_var_mem_shared |
                    nir_var_mem_global;
            break;
         case ir_intrinsic_memory_barrier_atomic_counter:
            /* There's no nir_var_atomic_counter, but since atomic counters are lowered
             * to SSBOs, we use nir_var_mem_ssbo instead.
             */
            scope = SCOPE_DEVICE;
            modes = nir_var_mem_ssbo;
            break;
         case ir_intrinsic_subgroup_memory_barrier:
            scope = SCOPE_SUBGROUP;
            modes = nir_var_image |
                    nir_var_mem_ssbo |
                    nir_var_mem_shared |
                    nir_var_mem_global;
            break;
         case ir_intrinsic_subgroup_memory_barrier_buffer:
            scope = SCOPE_SUBGROUP;
            modes = nir_var_mem_ssbo |
                    nir_var_mem_global;
            break;
         case ir_intrinsic_subgroup_memory_barrier_shared:
            scope = SCOPE_SUBGROUP;
            modes = nir_var_mem_shared;
            break;
         case ir_intrinsic_subgroup_memory_barrier_image:
            scope = SCOPE_SUBGROUP;
            modes = nir_var_image;
            break;
         default:
               UNREACHABLE("invalid intrinsic id for memory barrier");
         }

         nir_scoped_memory_barrier(&b, scope, NIR_MEMORY_ACQ_REL,
                                   (nir_variable_mode)modes);
         break;
      }
      case nir_intrinsic_store_ssbo: {
         ir_exec_node *param = ir->actual_parameters.get_head();
         ir_rvalue *block = ((ir_instruction *)param)->as_rvalue();

         param = param->get_next();
         ir_rvalue *offset = ((ir_instruction *)param)->as_rvalue();

         param = param->get_next();
         ir_rvalue *val = ((ir_instruction *)param)->as_rvalue();

         param = param->get_next();
         ir_constant *write_mask = ((ir_instruction *)param)->as_constant();
         assert(write_mask);

         nir_def *nir_val = evaluate_rvalue(val);
         if (glsl_type_is_boolean(val->type))
            nir_val = nir_b2i32(&b, nir_val);

         instr->src[0] = nir_src_for_ssa(nir_val);
         instr->src[1] = nir_src_for_ssa(evaluate_rvalue(block));
         instr->src[2] = nir_src_for_ssa(evaluate_rvalue(offset));
         intrinsic_set_std430_align(instr, val->type);
         nir_intrinsic_set_write_mask(instr, write_mask->value.u[0]);
         instr->num_components = val->type->vector_elements;

         nir_builder_instr_insert(&b, &instr->instr);
         break;
      }
      case nir_intrinsic_load_shared: {
         ir_exec_node *param = ir->actual_parameters.get_head();
         ir_rvalue *offset = ((ir_instruction *)param)->as_rvalue();

         nir_intrinsic_set_base(instr, 0);
         instr->src[0] = nir_src_for_ssa(evaluate_rvalue(offset));

         const glsl_type *type = ir->return_deref->var->type;
         instr->num_components = type->vector_elements;
         intrinsic_set_std430_align(instr, type);

         /* Setup destination register */
         unsigned bit_size = glsl_type_is_boolean(type) ? 32 : glsl_get_bit_size(type);
         nir_def_init(&instr->instr, &instr->def, type->vector_elements,
                      bit_size);

         nir_builder_instr_insert(&b, &instr->instr);

         /* The value in shared memory is a 32-bit value */
         if (glsl_type_is_boolean(type))
            ret = nir_b2b1(&b, &instr->def);
         break;
      }
      case nir_intrinsic_store_shared: {
         ir_exec_node *param = ir->actual_parameters.get_head();
         ir_rvalue *offset = ((ir_instruction *)param)->as_rvalue();

         param = param->get_next();
         ir_rvalue *val = ((ir_instruction *)param)->as_rvalue();

         param = param->get_next();
         ir_constant *write_mask = ((ir_instruction *)param)->as_constant();
         assert(write_mask);

         nir_intrinsic_set_base(instr, 0);
         instr->src[1] = nir_src_for_ssa(evaluate_rvalue(offset));

         nir_intrinsic_set_write_mask(instr, write_mask->value.u[0]);

         nir_def *nir_val = evaluate_rvalue(val);
         /* The value in shared memory is a 32-bit value */
         if (glsl_type_is_boolean(val->type))
            nir_val = nir_b2b32(&b, nir_val);

         instr->src[0] = nir_src_for_ssa(nir_val);
         instr->num_components = val->type->vector_elements;
         intrinsic_set_std430_align(instr, val->type);

         nir_builder_instr_insert(&b, &instr->instr);
         break;
      }
      case nir_intrinsic_reduce:
      case nir_intrinsic_inclusive_scan:
      case nir_intrinsic_exclusive_scan: {
         const glsl_type *type = ir->return_deref->type;
         nir_def_init(&instr->instr, &instr->def, glsl_get_vector_elements(type),
                      glsl_get_bit_size(type));
         instr->num_components = instr->def.num_components;

         ir_exec_node *param = ir->actual_parameters.get_head();
         ir_rvalue *value = ((ir_instruction *)param)->as_rvalue();
         instr->src[0] = nir_src_for_ssa(evaluate_rvalue(value));

         param = param->get_next();
         if (!param->is_tail_sentinel()) {
            ir_constant *size = ((ir_instruction *)param)->as_constant();
            assert(size);

            nir_intrinsic_set_cluster_size(instr, size->get_uint_component(0));
         }

         nir_intrinsic_set_reduction_op(instr, get_reduction_op(ir->callee->intrinsic_id, type));

         nir_builder_instr_insert(&b, &instr->instr);
         break;
      }
      case nir_intrinsic_shader_clock:
         nir_intrinsic_set_memory_scope(instr,
            ir->callee->intrinsic_id == ir_intrinsic_shader_clock ?
            SCOPE_SUBGROUP : SCOPE_DEVICE);
         FALLTHROUGH;
      case nir_intrinsic_begin_invocation_interlock:
      case nir_intrinsic_end_invocation_interlock:
      case nir_intrinsic_vote_ieq:
      case nir_intrinsic_vote_feq:
      case nir_intrinsic_vote_any:
      case nir_intrinsic_vote_all:
      case nir_intrinsic_ballot:
      case nir_intrinsic_read_invocation:
      case nir_intrinsic_read_first_invocation:
      case nir_intrinsic_is_helper_invocation:
      case nir_intrinsic_is_sparse_texels_resident:
      case nir_intrinsic_elect:
      case nir_intrinsic_inverse_ballot:
      case nir_intrinsic_ballot_bitfield_extract:
      case nir_intrinsic_ballot_bit_count_reduce:
      case nir_intrinsic_ballot_bit_count_inclusive:
      case nir_intrinsic_ballot_bit_count_exclusive:
      case nir_intrinsic_ballot_find_lsb:
      case nir_intrinsic_ballot_find_msb:
      case nir_intrinsic_shuffle:
      case nir_intrinsic_shuffle_xor:
      case nir_intrinsic_shuffle_up:
      case nir_intrinsic_shuffle_down:
      case nir_intrinsic_quad_broadcast:
      case nir_intrinsic_quad_swap_horizontal:
      case nir_intrinsic_quad_swap_vertical:
      case nir_intrinsic_quad_swap_diagonal: {
         if (ir->return_deref) {
            const glsl_type *type = ir->return_deref->type;
            nir_def_init(&instr->instr, &instr->def, glsl_get_vector_elements(type),
                         glsl_get_bit_size(type));

            if (!nir_intrinsic_dest_components(instr))
               instr->num_components = instr->def.num_components;
         }

         unsigned index = 0;
         ir_foreach_in_list(ir_rvalue, param, &ir->actual_parameters) {
            instr->src[index] = nir_src_for_ssa(evaluate_rvalue(param));

            if (!nir_intrinsic_src_components(instr, index))
               instr->num_components = nir_src_num_components(instr->src[index]);

            index++;
         }

         nir_builder_instr_insert(&b, &instr->instr);
         break;
      }
      default:
         UNREACHABLE("not reached");
      }

      if (ir->return_deref) {
         nir_deref_instr *ret_deref = evaluate_deref(ir->return_deref);

         if (op == nir_intrinsic_image_deref_sparse_load)
            adjust_sparse_variable(ret_deref, ir->return_deref->type, ret);

         nir_store_deref(&b, ret_deref, ret, ~0);
      }

      return;
   }

   struct hash_entry *entry =
      _mesa_hash_table_search(this->overload_table, ir->callee);
   assert(entry);
   nir_function *callee = (nir_function *) entry->data;

   nir_call_instr *call = nir_call_instr_create(this->shader, callee);

   unsigned i = 0;
   nir_deref_instr *ret_deref = NULL;
   if (ir->return_deref) {
      nir_variable *ret_tmp =
         nir_local_variable_create(this->impl, ir->return_deref->type,
                                   "return_tmp");
      ret_deref = nir_build_deref_var(&b, ret_tmp);
      call->params[i++] = nir_src_for_ssa(&ret_deref->def);
   }

   ir_foreach_two_lists(formal_node, &ir->callee->parameters,
                     actual_node, &ir->actual_parameters) {
      ir_rvalue *param_rvalue = (ir_rvalue *) actual_node;
      ir_variable *sig_param = (ir_variable *) formal_node;

      nir_deref_instr *param_deref;
      if (sig_param->data.mode == ir_var_function_in &&
          glsl_contains_opaque(sig_param->type)) {
         param_deref = evaluate_deref(param_rvalue);
      } else {
         nir_variable *param =
            nir_local_variable_create(this->impl, sig_param->type, "param");
         param->data.precision = sig_param->data.precision;
         param_deref = nir_build_deref_var(&b, param);

         if (sig_param->data.mode == ir_var_function_in ||
             sig_param->data.mode == ir_var_function_inout) {
            if (glsl_type_is_vector_or_scalar(param->type)) {
               nir_store_deref(&b, param_deref,
                               evaluate_rvalue(param_rvalue),
                               ~0);
            } else {
               nir_copy_deref(&b, param_deref, evaluate_deref(param_rvalue));
            }
         }
      }

      call->params[i] = nir_src_for_ssa(&param_deref->def);

      i++;
   }

   nir_builder_instr_insert(&b, &call->instr);

   /* Copy out params. We must do this after the function call to ensure we
    * do not overwrite global variables prematurely.
    */
   i = ir->return_deref ? 1 : 0;
   ir_foreach_two_lists(formal_node, &ir->callee->parameters,
                     actual_node, &ir->actual_parameters) {
      ir_rvalue *param_rvalue = (ir_rvalue *) actual_node;
      ir_variable *sig_param = (ir_variable *) formal_node;

      if (sig_param->data.mode == ir_var_function_out ||
          sig_param->data.mode == ir_var_function_inout) {
         if (glsl_type_is_vector_or_scalar(sig_param->type)) {
            nir_store_deref(&b, evaluate_deref(param_rvalue),
                            nir_load_deref(&b, nir_src_as_deref(call->params[i])),
                            ~0);
         } else {
            nir_copy_deref(&b, evaluate_deref(param_rvalue),
                           nir_src_as_deref(call->params[i]));
         }
      }

      i++;
   }


   if (ir->return_deref) {
      if (glsl_type_is_vector_or_scalar(ir->return_deref->type)) {
         nir_store_deref(&b, evaluate_deref(ir->return_deref),
                         nir_load_deref(&b, ret_deref), ~0);
      } else {
         nir_copy_deref(&b, evaluate_deref(ir->return_deref), ret_deref);
      }
   }
}

void
nir_visitor::visit(ir_assignment *ir)
{
   unsigned num_components = ir->lhs->type->vector_elements;
   unsigned write_mask = ir->write_mask;

   b.exact = ir->lhs->variable_referenced()->data.invariant ||
             ir->lhs->variable_referenced()->data.precise;

   if ((ir->rhs->as_dereference() || ir->rhs->as_constant()) &&
       (write_mask == BITFIELD_MASK(num_components) || write_mask == 0)) {
      nir_deref_instr *lhs = evaluate_deref(ir->lhs);
      nir_deref_instr *rhs = evaluate_deref(ir->rhs);
      enum gl_access_qualifier lhs_qualifiers = deref_get_qualifier(lhs);
      enum gl_access_qualifier rhs_qualifiers = deref_get_qualifier(rhs);

      nir_copy_deref_with_access(&b, lhs, rhs, lhs_qualifiers,
                                 rhs_qualifiers);
      return;
   }

   ir_texture *tex = ir->rhs->as_texture();
   bool is_sparse = tex && tex->is_sparse;

   if (!is_sparse)
      assert(glsl_type_is_scalar(ir->rhs->type) || glsl_type_is_vector(ir->rhs->type));

   ir->lhs->accept(this);
   nir_deref_instr *lhs_deref = this->deref;
   nir_def *src = evaluate_rvalue(ir->rhs);

   if (is_sparse) {
      adjust_sparse_variable(lhs_deref, tex->type, src);

      /* correct component and mask because they are 0 for struct */
      num_components = src->num_components;
      write_mask = BITFIELD_MASK(num_components);
   }

   if (write_mask != BITFIELD_MASK(num_components) && write_mask != 0) {
      /* GLSL IR will give us the input to the write-masked assignment in a
       * single packed vector.  So, for example, if the writemask is xzw, then
       * we have to swizzle x -> x, y -> z, and z -> w and get the y component
       * from the load.
       */
      unsigned swiz[4];
      unsigned component = 0;
      for (unsigned i = 0; i < 4; i++) {
         swiz[i] = write_mask & (1 << i) ? component++ : 0;
      }
      src = nir_swizzle(&b, src, swiz, num_components);
   }

   enum gl_access_qualifier qualifiers = deref_get_qualifier(lhs_deref);

   nir_store_deref_with_access(&b, lhs_deref, src, write_mask,
                               qualifiers);
}

/*
 * Given an instruction, returns a pointer to its destination or NULL if there
 * is no destination.
 *
 * Note that this only handles instructions we generate at this level.
 */
static nir_def *
get_instr_def(nir_instr *instr)
{
   nir_alu_instr *alu_instr;
   nir_intrinsic_instr *intrinsic_instr;
   nir_tex_instr *tex_instr;

   switch (instr->type) {
      case nir_instr_type_alu:
         alu_instr = nir_instr_as_alu(instr);
         return &alu_instr->def;

      case nir_instr_type_intrinsic:
         intrinsic_instr = nir_instr_as_intrinsic(instr);
         if (nir_intrinsic_infos[intrinsic_instr->intrinsic].has_dest)
            return &intrinsic_instr->def;
         else
            return NULL;

      case nir_instr_type_tex:
         tex_instr = nir_instr_as_tex(instr);
         return &tex_instr->def;

      default:
         UNREACHABLE("not reached");
   }

   return NULL;
}

void
nir_visitor::add_instr(nir_instr *instr, unsigned num_components,
                       unsigned bit_size)
{
   nir_def *def = get_instr_def(instr);

   if (def)
      nir_def_init(instr, def, num_components, bit_size);

   nir_builder_instr_insert(&b, instr);

   if (def)
      this->result = def;
}

nir_def *
nir_visitor::evaluate_rvalue(ir_rvalue* ir)
{
   ir->accept(this);
   if (ir->as_dereference() || ir->as_constant()) {
      /*
       * A dereference is being used on the right hand side, which means we
       * must emit a variable load.
       */

      enum gl_access_qualifier access = deref_get_qualifier(this->deref);
      this->result = nir_load_deref_with_access(&b, this->deref, access);
   }

   return this->result;
}

static bool
type_is_float(glsl_base_type type)
{
   return type == GLSL_TYPE_FLOAT || type == GLSL_TYPE_DOUBLE ||
      type == GLSL_TYPE_FLOAT16;
}

static bool
type_is_signed(glsl_base_type type)
{
   return type == GLSL_TYPE_INT || type == GLSL_TYPE_INT64 ||
      type == GLSL_TYPE_INT16;
}

void
nir_visitor::visit(ir_expression *ir)
{
   /* Some special cases */
   switch (ir->operation) {
   case ir_unop_interpolate_at_centroid:
   case ir_binop_interpolate_at_offset:
   case ir_binop_interpolate_at_sample: {
      ir_dereference *deref = ir->operands[0]->as_dereference();
      ir_swizzle *swizzle = NULL;
      ir_expression *precision_op = NULL;
      if (!deref) {
         precision_op = ir->operands[0]->as_expression();
         if (precision_op) {
            /* For some builtins precision is lowered to mediump for certain
             * parameters that ignore precision. For example for Interpolation
             * and Bitfield functions.
             */
            assert(precision_op->operation == ir_unop_f2fmp);
            deref = precision_op->operands[0]->as_dereference();
         }

         if (!deref) {
            swizzle = ir->operands[0]->as_swizzle();
            assert(swizzle);
            deref = swizzle->val->as_dereference();
         }

         assert(deref);
      }

      deref->accept(this);

      assert(nir_deref_mode_is(this->deref, nir_var_shader_in));
      nir_intrinsic_op op;
      switch (ir->operation) {
      case ir_unop_interpolate_at_centroid:
         op = nir_intrinsic_interp_deref_at_centroid;
         break;
      case ir_binop_interpolate_at_offset:
         op = nir_intrinsic_interp_deref_at_offset;
         break;
      case ir_binop_interpolate_at_sample:
         op = nir_intrinsic_interp_deref_at_sample;
         break;
      default:
         UNREACHABLE("Invalid interpolation intrinsic");
      }

      nir_intrinsic_instr *intrin = nir_intrinsic_instr_create(shader, op);
      intrin->num_components = deref->type->vector_elements;
      intrin->src[0] = nir_src_for_ssa(&this->deref->def);

      if (intrin->intrinsic == nir_intrinsic_interp_deref_at_offset ||
          intrin->intrinsic == nir_intrinsic_interp_deref_at_sample)
         intrin->src[1] = nir_src_for_ssa(evaluate_rvalue(ir->operands[1]));

      unsigned bit_size =  glsl_get_bit_size(deref->type);
      add_instr(&intrin->instr, deref->type->vector_elements, bit_size);

      if (swizzle) {
         unsigned swiz[4] = {
            swizzle->mask.x, swizzle->mask.y, swizzle->mask.z, swizzle->mask.w
         };

         result = nir_swizzle(&b, result, swiz,
                              swizzle->type->vector_elements);
      }

      if (precision_op) {
         result = nir_build_alu(&b, nir_op_f2fmp, result, NULL, NULL, NULL);
      }

      return;
   }

   case ir_unop_implicitly_sized_array_length:
   case ir_unop_ssbo_unsized_array_length: {
      nir_intrinsic_instr *intrin;
      if (ir->operation == ir_unop_ssbo_unsized_array_length) {
         intrin =
            nir_intrinsic_instr_create(b.shader,
                                       nir_intrinsic_deref_buffer_array_length);
      } else {
         intrin =
            nir_intrinsic_instr_create(b.shader,
                                       nir_intrinsic_deref_implicit_array_length);
      }

      ir_dereference *deref = ir->operands[0]->as_dereference();
      intrin->src[0] = nir_src_for_ssa(&evaluate_deref(deref)->def);

      add_instr(&intrin->instr, 1, 32);
      return;
   }

   default:
      break;
   }

   nir_def *srcs[4];
   for (unsigned i = 0; i < ir->num_operands; i++)
      srcs[i] = evaluate_rvalue(ir->operands[i]);

   glsl_base_type types[4];
   for (unsigned i = 0; i < ir->num_operands; i++)
      types[i] = ir->operands[i]->type->base_type;

   glsl_base_type out_type = ir->type->base_type;

   switch (ir->operation) {
   case ir_unop_bit_not: result = nir_inot(&b, srcs[0]); break;
   case ir_unop_logic_not:
      result = nir_inot(&b, srcs[0]);
      break;
   case ir_unop_neg:
      result = type_is_float(types[0]) ? nir_fneg(&b, srcs[0])
                                       : nir_ineg(&b, srcs[0]);
      break;
   case ir_unop_abs:
      result = type_is_float(types[0]) ? nir_fabs(&b, srcs[0])
                                       : nir_iabs(&b, srcs[0]);
      break;
   case ir_unop_clz:
      result = nir_uclz(&b, srcs[0]);
      break;
   case ir_unop_saturate:
      assert(type_is_float(types[0]));
      result = nir_fsat(&b, srcs[0]);
      break;
   case ir_unop_sign:
      result = type_is_float(types[0]) ? nir_fsign(&b, srcs[0])
                                       : nir_isign(&b, srcs[0]);
      break;
   case ir_unop_rcp:  result = nir_frcp(&b, srcs[0]);  break;
   case ir_unop_rsq:  result = nir_frsq(&b, srcs[0]);  break;
   case ir_unop_sqrt: result = nir_fsqrt(&b, srcs[0]); break;
   case ir_unop_exp:  result = nir_fexp2(&b, nir_fmul_imm(&b, srcs[0], M_LOG2E)); break;
   case ir_unop_log:  result = nir_fmul_imm(&b, nir_flog2(&b, srcs[0]), 1.0 / M_LOG2E); break;
   case ir_unop_exp2: result = nir_fexp2(&b, srcs[0]); break;
   case ir_unop_log2: result = nir_flog2(&b, srcs[0]); break;
   case ir_unop_i2f:
   case ir_unop_u2f:
   case ir_unop_b2f:
   case ir_unop_f2i:
   case ir_unop_f2u:
   case ir_unop_f2b:
   case ir_unop_i2b:
   case ir_unop_b2i:
   case ir_unop_b2i64:
   case ir_unop_d2f:
   case ir_unop_f2d:
   case ir_unop_f162u:
   case ir_unop_u2f16:
   case ir_unop_f162i:
   case ir_unop_i2f16:
   case ir_unop_f162f:
   case ir_unop_f2f16:
   case ir_unop_f162b:
   case ir_unop_b2f16:
   case ir_unop_f162d:
   case ir_unop_d2f16:
   case ir_unop_f162u64:
   case ir_unop_u642f16:
   case ir_unop_f162i64:
   case ir_unop_i642f16:
   case ir_unop_i2i:
   case ir_unop_u2u:
   case ir_unop_d2i:
   case ir_unop_d2u:
   case ir_unop_d2b:
   case ir_unop_i2d:
   case ir_unop_u2d:
   case ir_unop_i642i:
   case ir_unop_i642u:
   case ir_unop_i642f:
   case ir_unop_i642b:
   case ir_unop_i642d:
   case ir_unop_u642i:
   case ir_unop_u642u:
   case ir_unop_u642f:
   case ir_unop_u642d:
   case ir_unop_i2i64:
   case ir_unop_u2i64:
   case ir_unop_f2i64:
   case ir_unop_d2i64:
   case ir_unop_i2u64:
   case ir_unop_u2u64:
   case ir_unop_f2u64:
   case ir_unop_d2u64:
   case ir_unop_i2u:
   case ir_unop_u2i:
   case ir_unop_i642u64:
   case ir_unop_u642i64: {
      nir_alu_type src_type = nir_get_nir_type_for_glsl_base_type(types[0]);
      nir_alu_type dst_type = nir_get_nir_type_for_glsl_base_type(out_type);
      result = nir_type_convert(&b, srcs[0], src_type, dst_type,
                                nir_rounding_mode_undef);
      /* b2i and b2f don't have fixed bit-size versions so the builder will
       * just assume 32 and we have to fix it up here.
       */
      result->bit_size = nir_alu_type_get_type_size(dst_type);
      break;
   }

   case ir_unop_f2fmp: {
      result = nir_build_alu(&b, nir_op_f2fmp, srcs[0], NULL, NULL, NULL);
      break;
   }

   case ir_unop_i2imp: {
      result = nir_build_alu(&b, nir_op_i2imp, srcs[0], NULL, NULL, NULL);
      break;
   }

   case ir_unop_u2ump: {
      result = nir_build_alu(&b, nir_op_i2imp, srcs[0], NULL, NULL, NULL);
      break;
   }

   case ir_unop_bitcast_i2f:
   case ir_unop_bitcast_f2i:
   case ir_unop_bitcast_u2f:
   case ir_unop_bitcast_f2u:
   case ir_unop_bitcast_i642d:
   case ir_unop_bitcast_d2i64:
   case ir_unop_bitcast_u642d:
   case ir_unop_bitcast_d2u64:
   case ir_unop_subroutine_to_int:
      /* no-op */
      result = nir_mov(&b, srcs[0]);
      break;
   case ir_unop_trunc: result = nir_ftrunc(&b, srcs[0]); break;
   case ir_unop_ceil:  result = nir_fceil(&b, srcs[0]); break;
   case ir_unop_floor: result = nir_ffloor(&b, srcs[0]); break;
   case ir_unop_fract: result = nir_ffract(&b, srcs[0]); break;
   case ir_unop_frexp_exp: result = nir_frexp_exp(&b, srcs[0]); break;
   case ir_unop_frexp_sig: result = nir_frexp_sig(&b, srcs[0]); break;
   case ir_unop_round_even: result = nir_fround_even(&b, srcs[0]); break;
   case ir_unop_sin:   result = nir_fsin(&b, srcs[0]); break;
   case ir_unop_cos:   result = nir_fcos(&b, srcs[0]); break;
   case ir_unop_dFdx:        result = nir_ddx(&b, srcs[0]); break;
   case ir_unop_dFdy:        result = nir_ddy(&b, srcs[0]); break;
   case ir_unop_dFdx_fine:   result = nir_ddx_fine(&b, srcs[0]); break;
   case ir_unop_dFdy_fine:   result = nir_ddy_fine(&b, srcs[0]); break;
   case ir_unop_dFdx_coarse: result = nir_ddx_coarse(&b, srcs[0]); break;
   case ir_unop_dFdy_coarse: result = nir_ddy_coarse(&b, srcs[0]); break;
   case ir_unop_pack_snorm_2x16:
      result = nir_pack_snorm_2x16(&b, srcs[0]);
      break;
   case ir_unop_pack_snorm_4x8:
      result = nir_pack_snorm_4x8(&b, srcs[0]);
      break;
   case ir_unop_pack_unorm_2x16:
      result = nir_pack_unorm_2x16(&b, srcs[0]);
      break;
   case ir_unop_pack_unorm_4x8:
      result = nir_pack_unorm_4x8(&b, srcs[0]);
      break;
   case ir_unop_pack_half_2x16:
      result = nir_pack_half_2x16(&b, srcs[0]);
      break;
   case ir_unop_unpack_snorm_2x16:
      result = nir_unpack_snorm_2x16(&b, srcs[0]);
      break;
   case ir_unop_unpack_snorm_4x8:
      result = nir_unpack_snorm_4x8(&b, srcs[0]);
      break;
   case ir_unop_unpack_unorm_2x16:
      result = nir_unpack_unorm_2x16(&b, srcs[0]);
      break;
   case ir_unop_unpack_unorm_4x8:
      result = nir_unpack_unorm_4x8(&b, srcs[0]);
      break;
   case ir_unop_unpack_half_2x16:
      result = nir_unpack_half_2x16(&b, srcs[0]);
      break;
   case ir_unop_pack_sampler_2x32:
   case ir_unop_pack_image_2x32:
   case ir_unop_pack_double_2x32:
   case ir_unop_pack_int_2x32:
   case ir_unop_pack_uint_2x32:
      result = nir_pack_64_2x32(&b, srcs[0]);
      break;
   case ir_unop_unpack_sampler_2x32:
   case ir_unop_unpack_image_2x32:
   case ir_unop_unpack_double_2x32:
   case ir_unop_unpack_int_2x32:
   case ir_unop_unpack_uint_2x32:
      result = nir_unpack_64_2x32(&b, srcs[0]);
      break;
   case ir_unop_bitfield_reverse:
      result = nir_bitfield_reverse(&b, srcs[0]);
      break;
   case ir_unop_bit_count:
      result = nir_bit_count(&b, srcs[0]);
      break;
   case ir_unop_find_msb:
      switch (types[0]) {
      case GLSL_TYPE_UINT:
         result = nir_ufind_msb(&b, srcs[0]);
         break;
      case GLSL_TYPE_INT:
         result = nir_ifind_msb(&b, srcs[0]);
         break;
      default:
         UNREACHABLE("Invalid type for findMSB()");
      }
      break;
   case ir_unop_find_lsb:
      result = nir_find_lsb(&b, srcs[0]);
      break;

   case ir_unop_get_buffer_size: {
      nir_intrinsic_instr *load = nir_intrinsic_instr_create(
         this->shader,
         nir_intrinsic_get_ssbo_size);
      load->num_components = ir->type->vector_elements;
      load->src[0] = nir_src_for_ssa(evaluate_rvalue(ir->operands[0]));
      unsigned bit_size = glsl_get_bit_size(ir->type);
      add_instr(&load->instr, ir->type->vector_elements, bit_size);
      return;
   }

   case ir_unop_atan:
      result = nir_atan(&b, srcs[0]);
      break;

   case ir_binop_add:
      result = type_is_float(out_type) ? nir_fadd(&b, srcs[0], srcs[1])
                                       : nir_iadd(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_add_sat:
      result = type_is_signed(out_type) ? nir_iadd_sat(&b, srcs[0], srcs[1])
                                        : nir_uadd_sat(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_sub:
      result = type_is_float(out_type) ? nir_fsub(&b, srcs[0], srcs[1])
                                       : nir_isub(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_sub_sat:
      result = type_is_signed(out_type) ? nir_isub_sat(&b, srcs[0], srcs[1])
                                        : nir_usub_sat(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_abs_sub:
      /* out_type is always unsigned for ir_binop_abs_sub, so we have to key
       * on the type of the sources.
       */
      result = type_is_signed(types[0]) ? nir_uabs_isub(&b, srcs[0], srcs[1])
                                        : nir_uabs_usub(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_avg:
      result = type_is_signed(out_type) ? nir_ihadd(&b, srcs[0], srcs[1])
                                        : nir_uhadd(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_avg_round:
      result = type_is_signed(out_type) ? nir_irhadd(&b, srcs[0], srcs[1])
                                        : nir_urhadd(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_mul_32x16:
      result = type_is_signed(out_type) ? nir_imul_32x16(&b, srcs[0], srcs[1])
                                        : nir_umul_32x16(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_mul:
      if (type_is_float(out_type))
         result = nir_fmul(&b, srcs[0], srcs[1]);
      else if (out_type == GLSL_TYPE_INT64 &&
               (ir->operands[0]->type->base_type == GLSL_TYPE_INT ||
                ir->operands[1]->type->base_type == GLSL_TYPE_INT))
         result = nir_imul_2x32_64(&b, srcs[0], srcs[1]);
      else if (out_type == GLSL_TYPE_UINT64 &&
               (ir->operands[0]->type->base_type == GLSL_TYPE_UINT ||
                ir->operands[1]->type->base_type == GLSL_TYPE_UINT))
         result = nir_umul_2x32_64(&b, srcs[0], srcs[1]);
      else
         result = nir_imul(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_div:
      if (type_is_float(out_type))
         result = nir_fdiv(&b, srcs[0], srcs[1]);
      else if (type_is_signed(out_type))
         result = nir_idiv(&b, srcs[0], srcs[1]);
      else
         result = nir_udiv(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_mod:
      result = type_is_float(out_type) ? nir_fmod(&b, srcs[0], srcs[1])
                                       : nir_umod(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_min:
      if (type_is_float(out_type))
         result = nir_fmin(&b, srcs[0], srcs[1]);
      else if (type_is_signed(out_type))
         result = nir_imin(&b, srcs[0], srcs[1]);
      else
         result = nir_umin(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_max:
      if (type_is_float(out_type))
         result = nir_fmax(&b, srcs[0], srcs[1]);
      else if (type_is_signed(out_type))
         result = nir_imax(&b, srcs[0], srcs[1]);
      else
         result = nir_umax(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_pow: result = nir_fpow(&b, srcs[0], srcs[1]); break;
   case ir_binop_bit_and: result = nir_iand(&b, srcs[0], srcs[1]); break;
   case ir_binop_bit_or: result = nir_ior(&b, srcs[0], srcs[1]); break;
   case ir_binop_bit_xor: result = nir_ixor(&b, srcs[0], srcs[1]); break;
   case ir_binop_logic_and:
      result = nir_iand(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_logic_or:
      result = nir_ior(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_logic_xor:
      result = nir_ixor(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_lshift: result = nir_ishl(&b, srcs[0], nir_u2u32(&b, srcs[1])); break;
   case ir_binop_rshift:
      result = (type_is_signed(out_type)) ? nir_ishr(&b, srcs[0], nir_u2u32(&b, srcs[1]))
                                          : nir_ushr(&b, srcs[0], nir_u2u32(&b, srcs[1]));
      break;
   case ir_binop_imul_high:
      result = (out_type == GLSL_TYPE_INT) ? nir_imul_high(&b, srcs[0], srcs[1])
                                           : nir_umul_high(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_carry:  result = nir_uadd_carry(&b, srcs[0], srcs[1]);  break;
   case ir_binop_borrow: result = nir_usub_borrow(&b, srcs[0], srcs[1]); break;
   case ir_binop_less:
      if (type_is_float(types[0]))
         result = nir_flt(&b, srcs[0], srcs[1]);
      else if (type_is_signed(types[0]))
         result = nir_ilt(&b, srcs[0], srcs[1]);
      else
         result = nir_ult(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_gequal:
      if (type_is_float(types[0]))
         result = nir_fge(&b, srcs[0], srcs[1]);
      else if (type_is_signed(types[0]))
         result = nir_ige(&b, srcs[0], srcs[1]);
      else
         result = nir_uge(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_equal:
      if (type_is_float(types[0]))
         result = nir_feq(&b, srcs[0], srcs[1]);
      else
         result = nir_ieq(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_nequal:
      if (type_is_float(types[0]))
         result = nir_fneu(&b, srcs[0], srcs[1]);
      else
         result = nir_ine(&b, srcs[0], srcs[1]);
      break;
   case ir_binop_all_equal:
      if (type_is_float(types[0])) {
         switch (ir->operands[0]->type->vector_elements) {
            case 1: result = nir_feq(&b, srcs[0], srcs[1]); break;
            case 2: result = nir_ball_fequal2(&b, srcs[0], srcs[1]); break;
            case 3: result = nir_ball_fequal3(&b, srcs[0], srcs[1]); break;
            case 4: result = nir_ball_fequal4(&b, srcs[0], srcs[1]); break;
            default:
               UNREACHABLE("not reached");
         }
      } else {
         switch (ir->operands[0]->type->vector_elements) {
            case 1: result = nir_ieq(&b, srcs[0], srcs[1]); break;
            case 2: result = nir_ball_iequal2(&b, srcs[0], srcs[1]); break;
            case 3: result = nir_ball_iequal3(&b, srcs[0], srcs[1]); break;
            case 4: result = nir_ball_iequal4(&b, srcs[0], srcs[1]); break;
            default:
               UNREACHABLE("not reached");
         }
      }
      break;
   case ir_binop_any_nequal:
      if (type_is_float(types[0])) {
         switch (ir->operands[0]->type->vector_elements) {
            case 1: result = nir_fneu(&b, srcs[0], srcs[1]); break;
            case 2: result = nir_bany_fnequal2(&b, srcs[0], srcs[1]); break;
            case 3: result = nir_bany_fnequal3(&b, srcs[0], srcs[1]); break;
            case 4: result = nir_bany_fnequal4(&b, srcs[0], srcs[1]); break;
            default:
               UNREACHABLE("not reached");
         }
      } else {
         switch (ir->operands[0]->type->vector_elements) {
            case 1: result = nir_ine(&b, srcs[0], srcs[1]); break;
            case 2: result = nir_bany_inequal2(&b, srcs[0], srcs[1]); break;
            case 3: result = nir_bany_inequal3(&b, srcs[0], srcs[1]); break;
            case 4: result = nir_bany_inequal4(&b, srcs[0], srcs[1]); break;
            default:
               UNREACHABLE("not reached");
         }
      }
      break;
   case ir_binop_dot:
      result = nir_fdot(&b, srcs[0], srcs[1]);
      break;

   case ir_binop_vector_extract:
      result = nir_vector_extract(&b, srcs[0], srcs[1]);
      break;
   case ir_triop_vector_insert:
      result = nir_vector_insert(&b, srcs[0], srcs[1], srcs[2]);
      break;

   case ir_binop_atan2:
      result = nir_atan2(&b, srcs[0], srcs[1]);
      break;

   case ir_binop_ldexp: result = nir_ldexp(&b, srcs[0], srcs[1]); break;
   case ir_triop_fma:
      result = nir_ffma(&b, srcs[0], srcs[1], srcs[2]);
      break;
   case ir_triop_lrp:
      result = nir_flrp(&b, srcs[0], srcs[1], srcs[2]);
      break;
   case ir_triop_csel:
      result = nir_bcsel(&b, srcs[0], srcs[1], srcs[2]);
      break;
   case ir_triop_bitfield_extract:
      result = glsl_type_is_int_16_32(ir->type) ?
         nir_ibitfield_extract(&b, nir_i2i32(&b, srcs[0]), nir_i2i32(&b, srcs[1]), nir_i2i32(&b, srcs[2])) :
         nir_ubitfield_extract(&b, nir_u2u32(&b, srcs[0]), nir_i2i32(&b, srcs[1]), nir_i2i32(&b, srcs[2]));

      if (ir->type->base_type == GLSL_TYPE_INT16) {
         result = nir_i2i16(&b, result);
      } else if (ir->type->base_type == GLSL_TYPE_UINT16) {
         result = nir_u2u16(&b, result);
      }

      break;
   case ir_quadop_bitfield_insert:
      result = nir_bitfield_insert(&b,
                                   nir_u2u32(&b, srcs[0]), nir_u2u32(&b, srcs[1]),
                                   nir_i2i32(&b, srcs[2]), nir_i2i32(&b, srcs[3]));

      if (ir->type->base_type == GLSL_TYPE_INT16) {
         result = nir_i2i16(&b, result);
      } else if (ir->type->base_type == GLSL_TYPE_UINT16) {
         result = nir_u2u16(&b, result);
      }

      break;
   case ir_quadop_vector:
      result = nir_vec(&b, srcs, ir->type->vector_elements);
      break;

   default:
      UNREACHABLE("not reached");
   }

   /* The bit-size of the NIR SSA value must match the bit-size of the
    * original GLSL IR expression.
    */
   assert(result->bit_size == glsl_base_type_get_bit_size(ir->type->base_type));
}

void
nir_visitor::visit(ir_swizzle *ir)
{
   unsigned swizzle[4] = { ir->mask.x, ir->mask.y, ir->mask.z, ir->mask.w };
   result = nir_swizzle(&b, evaluate_rvalue(ir->val), swizzle,
                        ir->type->vector_elements);
}

void
nir_visitor::visit(ir_texture *ir)
{
   unsigned num_srcs;
   nir_texop op;
   switch (ir->op) {
   case ir_tex:
      op = nir_texop_tex;
      num_srcs = 1; /* coordinate */
      break;

   case ir_txb:
   case ir_txl:
      op = (ir->op == ir_txb) ? nir_texop_txb : nir_texop_txl;
      num_srcs = 2; /* coordinate, bias/lod */
      break;

   case ir_txd:
      op = nir_texop_txd; /* coordinate, dPdx, dPdy */
      num_srcs = 3;
      break;

   case ir_txf:
      op = nir_texop_txf;
      if (ir->lod_info.lod != NULL)
         num_srcs = 2; /* coordinate, lod */
      else
         num_srcs = 1; /* coordinate */
      break;

   case ir_txf_ms:
      op = nir_texop_txf_ms;
      num_srcs = 2; /* coordinate, sample_index */
      break;

   case ir_txs:
      op = nir_texop_txs;
      if (ir->lod_info.lod != NULL)
         num_srcs = 1; /* lod */
      else
         num_srcs = 0;
      break;

   case ir_lod:
      op = nir_texop_lod;
      num_srcs = 1; /* coordinate */
      break;

   case ir_tg4:
      op = nir_texop_tg4;
      num_srcs = 1; /* coordinate */
      break;

   case ir_query_levels:
      op = nir_texop_query_levels;
      num_srcs = 0;
      break;

   case ir_texture_samples:
      op = nir_texop_texture_samples;
      num_srcs = 0;
      break;

   case ir_samples_identical:
      op = nir_texop_samples_identical;
      num_srcs = 1; /* coordinate */
      break;

   default:
      UNREACHABLE("not reached");
   }

   if (ir->projector != NULL)
      num_srcs++;
   if (ir->shadow_comparator != NULL)
      num_srcs++;
   /* offsets are constants we store inside nir_tex_intrs.offsets */
   if (ir->offset != NULL && !glsl_type_is_array(ir->offset->type))
      num_srcs++;
   if (ir->clamp != NULL)
      num_srcs++;

   /* Add one for the texture deref */
   num_srcs += 2;

   nir_tex_instr *instr = nir_tex_instr_create(this->shader, num_srcs);

   instr->op = op;
   instr->sampler_dim =
      (glsl_sampler_dim) ir->sampler->type->sampler_dimensionality;
   instr->is_array = ir->sampler->type->sampler_array;
   instr->is_shadow = ir->sampler->type->sampler_shadow;

   const glsl_type *dest_type
      = ir->is_sparse ? glsl_get_field_type(ir->type, "texel") : ir->type;
   assert(dest_type != &glsl_type_builtin_error);
   if (instr->is_shadow)
      instr->is_new_style_shadow = (dest_type->vector_elements == 1);
   instr->dest_type = nir_get_nir_type_for_glsl_type(dest_type);
   instr->is_sparse = ir->is_sparse;

   nir_deref_instr *sampler_deref = evaluate_deref(ir->sampler);
   nir_def *tex_intrin = nir_deref_texture_src(&b, 32, &sampler_deref->def);

   instr->src[0] = nir_tex_src_for_ssa(nir_tex_src_sampler_deref_intrinsic,
                                       tex_intrin);
   instr->src[1] = nir_tex_src_for_ssa(nir_tex_src_texture_deref_intrinsic,
                                       tex_intrin);

   unsigned src_number = 2;

   if (ir->coordinate != NULL) {
      instr->coord_components = ir->coordinate->type->vector_elements;
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_coord,
                                                   evaluate_rvalue(ir->coordinate));
      src_number++;
   }

   if (ir->projector != NULL) {
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_projector,
                                                   evaluate_rvalue(ir->projector));
      src_number++;
   }

   if (ir->shadow_comparator != NULL) {
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_comparator,
                                                   evaluate_rvalue(ir->shadow_comparator));
      src_number++;
   }

   if (ir->offset != NULL) {
      if (glsl_type_is_array(ir->offset->type)) {
         const int size = MIN2(glsl_array_size(ir->offset->type), 4);
         for (int i = 0; i < size; i++) {
            const ir_constant *c =
               ir->offset->as_constant()->get_array_element(i);

            for (unsigned j = 0; j < 2; ++j) {
               int val = c->get_int_component(j);
               instr->tg4_offsets[i][j] = val;
            }
         }
      } else {
         assert(glsl_type_is_vector(ir->offset->type) || glsl_type_is_scalar(ir->offset->type));

         instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_offset,
                                                      evaluate_rvalue(ir->offset));
         src_number++;
      }
   }

   if (ir->clamp) {
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_min_lod,
                                                   evaluate_rvalue(ir->clamp));
      src_number++;
   }

   switch (ir->op) {
   case ir_txb:
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_bias,
                                                   evaluate_rvalue(ir->lod_info.bias));
      src_number++;
      break;

   case ir_txl:
   case ir_txf:
   case ir_txs:
      if (ir->lod_info.lod != NULL) {
         instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_lod,
                                                      evaluate_rvalue(ir->lod_info.lod));
         src_number++;
      }
      break;

   case ir_txd:
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_ddx,
                                                   evaluate_rvalue(ir->lod_info.grad.dPdx));
      src_number++;
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_ddy,
                                                   evaluate_rvalue(ir->lod_info.grad.dPdy));
      src_number++;
      break;

   case ir_txf_ms:
      instr->src[src_number] = nir_tex_src_for_ssa(nir_tex_src_ms_index,
                                                   evaluate_rvalue(ir->lod_info.sample_index));
      src_number++;
      break;

   case ir_tg4:
      instr->component = ir->lod_info.component->as_constant()->value.u[0];
      break;

   default:
      break;
   }

   assert(src_number == num_srcs);

   unsigned bit_size = glsl_get_bit_size(dest_type);
   add_instr(&instr->instr, nir_tex_instr_dest_size(instr), bit_size);
}

void
nir_visitor::visit(ir_constant *ir)
{
   /*
    * We don't know if this variable is an array or struct that gets
    * dereferenced, so do the safe thing an make it a variable with a
    * constant initializer and return a dereference.
    */

   nir_variable *var =
      nir_local_variable_create(this->impl, ir->type, "const_temp");
   var->data.read_only = true;
   var->constant_initializer = constant_copy(ir, var);

   this->deref = nir_build_deref_var(&b, var);
}

void
nir_visitor::visit(ir_dereference_variable *ir)
{
   if (ir->variable_referenced()->data.mode == ir_var_function_out ||
       ir->variable_referenced()->data.mode == ir_var_function_inout ||
       ir->variable_referenced()->data.mode == ir_var_function_in) {
      unsigned i = (sig->return_type != &glsl_type_builtin_void) ? 1 : 0;

      ir_foreach_in_list(ir_variable, param, &sig->parameters) {
         if (param == ir->variable_referenced()) {
            break;
         }
         i++;
      }

      this->deref = nir_build_deref_cast(&b, nir_load_param(&b, i),
                                         nir_var_function_temp, ir->type, 0);
      return;
   }

   struct hash_entry *entry =
      _mesa_hash_table_search(this->var_table, ir->var);
   assert(entry);
   nir_variable *var = (nir_variable *) entry->data;

   this->deref = nir_build_deref_var(&b, var);
}

void
nir_visitor::visit(ir_dereference_record *ir)
{
   ir->record->accept(this);

   int field_index = ir->field_idx;
   assert(field_index >= 0);

   /* sparse texture variable is a struct for ir_variable, but it has been
    * converted to a vector for nir_variable.
    */
   if (this->deref->deref_type == nir_deref_type_var &&
       _mesa_set_search(this->sparse_variable_set, this->deref->var)) {
      nir_def *load = nir_load_deref(&b, this->deref);
      assert(load->num_components >= 2);

      nir_def *ssa;
      const glsl_type *type = ir->record->type;
      if (field_index == glsl_get_field_index(type, "code")) {
         /* last channel holds residency code */
         ssa = nir_channel(&b, load, load->num_components - 1);
      } else {
         assert(field_index == glsl_get_field_index(type, "texel"));

         unsigned mask = BITFIELD_MASK(load->num_components - 1);
         ssa = nir_channels(&b, load, mask);
      }

      /* still need to create a deref for return */
      nir_variable *tmp =
         nir_local_variable_create(this->impl, ir->type, "deref_tmp");
      this->deref = nir_build_deref_var(&b, tmp);
      nir_store_deref(&b, this->deref, ssa, ~0);
   } else
      this->deref = nir_build_deref_struct(&b, this->deref, field_index);
}

void
nir_visitor::visit(ir_dereference_array *ir)
{
   nir_def *index = evaluate_rvalue(ir->array_index);

   /* In NIR, array indices must match the pointer size (32-bits for arrays),
    * but in GLSL IR, we might "optimize" the array index to a 16-bit integer.
    * We need to upcast in that case to keep the NIR valid.
    */
   index = nir_i2iN(&b, index, this->deref->def.bit_size);

   ir->array->accept(this);

   this->deref = nir_build_deref_array(&b, this->deref, index);
}

void
nir_visitor::visit(ir_barrier *)
{
   if (shader->info.stage == MESA_SHADER_COMPUTE) {
      nir_barrier(&b, SCOPE_WORKGROUP, SCOPE_WORKGROUP,
                      NIR_MEMORY_ACQ_REL, nir_var_mem_shared);
   } else if (shader->info.stage == MESA_SHADER_TESS_CTRL) {
      nir_barrier(&b, SCOPE_WORKGROUP, SCOPE_WORKGROUP,
                      NIR_MEMORY_ACQ_REL, nir_var_shader_out);
   }
}

nir_shader *
glsl_float64_funcs_to_nir(struct gl_context *ctx,
                          const nir_shader_compiler_options *options)
{
   /* We pretend it's a vertex shader.  Ultimately, the stage shouldn't
    * matter because we're not optimizing anything here.
    */
   struct gl_shader *sh = _mesa_new_shader(-1, MESA_SHADER_VERTEX);
   sh->Source = float64_source;
   sh->CompileStatus = COMPILE_FAILURE;
   _mesa_glsl_compile_shader(ctx, sh, NULL, false, false, true);
   nir_shader *nir = nir_shader_clone(NULL, sh->nir);

   if (!sh->CompileStatus) {
      if (sh->InfoLog) {
         _mesa_problem(ctx,
                       "fp64 software impl compile failed:\n%s\nsource:\n%s\n",
                       sh->InfoLog, float64_source);
      }
      return NULL;
   }

   /* _mesa_delete_shader will try to free sh->Source but it's static const */
   sh->Source = NULL;
   _mesa_delete_shader(ctx, sh);

   nir_validate_shader(nir, "float64_funcs_to_nir");

   NIR_PASS(_, nir, nir_lower_variable_initializers, nir_var_function_temp);
   NIR_PASS(_, nir, nir_lower_returns);
   NIR_PASS(_, nir, nir_inline_functions);
   NIR_PASS(_, nir, nir_opt_deref);

   /* Do some optimizations to clean up the shader now.  By optimizing the
    * functions in the library, we avoid having to re-do that work every
    * time we inline a copy of a function.  Reducing basic blocks also helps
    * with compile times.
    */
   NIR_PASS(_, nir, nir_lower_vars_to_ssa);
   NIR_PASS(_, nir, nir_remove_dead_variables, nir_var_function_temp, NULL);
   NIR_PASS(_, nir, nir_copy_prop);
   NIR_PASS(_, nir, nir_opt_dce);
   NIR_PASS(_, nir, nir_opt_cse);
   NIR_PASS(_, nir, nir_opt_gcm, true);

   nir_opt_peephole_select_options peephole_select_options = {};
   peephole_select_options.limit = 1;
   NIR_PASS(_, nir, nir_opt_peephole_select, &peephole_select_options);
   NIR_PASS(_, nir, nir_opt_dce);

   return nir;
}
