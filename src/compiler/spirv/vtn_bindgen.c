/*
 * Copyright 2024 Valve Corporation
 * Copyright 2023 Alyssa Rosenzweig
 * SPDX-License-Identifier: MIT
 */

#include "compiler/spirv/nir_spirv.h"

static const struct spirv_to_nir_options spirv_options = {
   .environment = NIR_SPIRV_OPENCL,
   .shared_addr_format = nir_address_format_62bit_generic,
   .global_addr_format = nir_address_format_62bit_generic,
   .temp_addr_format = nir_address_format_62bit_generic,
   .constant_addr_format = nir_address_format_64bit_global,
   .create_library = true,
};

int
main(int argc, char **argv)
{
   if (argc != 3) {
      fprintf(stderr, "Usage: %s [input spir-v] [output header]\n", argv[0]);
      return 1;
   }

   const char *infile = argv[1];
   const char *outfile = argv[2];

   FILE *fin = fopen(infile, "rb");
   if (!fin) {
      fprintf(stderr, "Failed to open %s\n", infile);
      return 1;
   }

   fseek(fin, 0L, SEEK_END);
   size_t len = ftell(fin);
   rewind(fin);

   uint32_t *map = malloc(ALIGN_POT(len, 4));
   if (!map) {
      fprintf(stderr, "Failed to allocate");
      fclose(fin);
      return 1;
   }

   fread(map, 1, len, fin);
   fclose(fin);

   FILE *fout = fopen(outfile, "w");
   if (!fout) {
      fprintf(stderr, "Failed to open %s\n", outfile);
      free(map);
      return 1;
   }

   glsl_type_singleton_init_or_ref();

   fprintf(fout, "/*\n");
   fprintf(fout, " * Copyright Mesa3D Contributors\n");
   fprintf(fout, " * SPDX-License-Identifier: MIT\n");
   fprintf(fout, " *\n");
   fprintf(fout, " * Autogenerated file, do not edit\n");
   fprintf(fout, " */\n");
   fprintf(fout, "#pragma once\n");
   fprintf(fout, "#include <stdint.h>\n");

   spirv_library_to_nir_builder(fout, map, len / 4, &spirv_options);

   glsl_type_singleton_decref();
   fclose(fout);
   free(map);
   return 0;
}