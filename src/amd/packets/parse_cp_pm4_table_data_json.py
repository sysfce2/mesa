#!/usr/bin/env python3
#
# Copyright 2026 Advanced Micro Devices, Inc.
# SPDX-License-Identifier: MIT

"""
The parameters must be specified in the following order and must contain 'gfx'. The gfx version
must be the last parameter. Only the header file for the specified gfx version is generated.
All other input files are only used to resolve definition conflicts. The generated header file
is written to stdout.

Parameters:
    cp_pm4_table_data_gfx$1.json
    pm4_it_opcodes_gfx$1.h
    ...
    cp_pm4_table_data_gfx$N.json
    pm4_it_opcodes_gfx$N.h
    gfx$VERSION (e.g. 'gfx11')

"""

import sys, json, re

engines_dict = {'pfp': 0, 'meg': 1, 'mec': 2}


def gfx_version_to_int(s):
    assert s[0:3] == 'gfx'
    return int(s[3:])


def engine_to_int(s):
    assert s in engines_dict
    return engines_dict[s]


def int_to_engine(i):
    # Swap keys and values
    rev_dict = {v: k for k, v in engines_dict.items()}
    assert i in rev_dict
    return rev_dict[i]


# Return (suffix, comment) that should be added to the definition by detecting content conflicts
# in list = [(gfx_version_int, engine_int, content), ...]. Other parameters determine which
# definition this is for.
def get_conflict_suffix(list, in_gfx_version_name, in_engine_name, in_content):
    assert len(list) > 0

    in_gfx_version = gfx_version_to_int(in_gfx_version_name)
    in_engine = engine_to_int(in_engine_name)

    # Are there any conflicts?
    any_conflict = False

    for _, _, content in list:
        _, _, first_content = list[0]
        if first_content != content:
            any_conflict = True
            break

    if not any_conflict:
        return ('', '')

    # Are there any conflicts between engines of the same gfx version?
    any_engine_conflict = False
    matching_engines = set()

    first = {}
    for gfx_version, engine, content in list:
        if content == in_content:
            matching_engines.add(engine)

        if gfx_version not in first:
            first[gfx_version] = content
        elif first[gfx_version] != content:
            any_engine_conflict = True

    # Are there any conflicts between gfx versions of the same engine?
    any_gfx_conflict = False
    first_matching_gfx_version = -1
    matching_gfx_versions = set()

    first = {}
    for gfx_version, engine, content in list:
        if content == in_content:
            matching_gfx_versions.add(gfx_version)
            if first_matching_gfx_version == -1 and gfx_version <= in_gfx_version:
                first_matching_gfx_version = gfx_version

        if engine not in first:
            first[engine] = content
        elif first[engine] != content:
            any_gfx_conflict = True

    # Assemble the name suffix and comment for the definition
    assert any_gfx_conflict or any_engine_conflict
    suffix = ((('_gfx%d' % first_matching_gfx_version) if any_gfx_conflict else '') +
              (('_%s' % in_engine_name) if any_engine_conflict else ''))

    comment = ' /* '
    if any_gfx_conflict:
        comment += ', '.join([('gfx%d' % x) for x in matching_gfx_versions])
    if any_gfx_conflict and any_engine_conflict:
        comment += ' | '
    if any_engine_conflict:
        comment += ', '.join([int_to_engine(x) for x in matching_engines])
    comment += ' */'

    return (suffix, comment)


# Return (suffix, comment) for the given packet.
def get_packet_conflict_suffix(gfx_opcodes, in_gfx_version_name, in_packet_name, in_opcode):
    # Gather an array of tuples (gfx_version_int, content) for the packet.
    list = []

    for gfx_version_name, opcodes in gfx_opcodes.items():
        if in_packet_name in opcodes:
            list += [(gfx_version_to_int(gfx_version_name), 0, opcodes[in_packet_name])]

    return get_conflict_suffix(list, in_gfx_version_name, int_to_engine(0), in_opcode)


# Return (suffix, comment) for the given packet field.
def get_field_conflict_suffix(gfx_versions, in_gfx_version_name, in_engine_name,
                              in_packet_name, in_word_index, in_word_variant_name,
                              in_field_name, in_field_bits):
    # Gather an array of tuples (gfx_version_int, engine_int, content) for the field.
    list = []
    for gfx_version_name, engines in gfx_versions.items():
        for engine_name, packets in engines.items():
            if in_packet_name in packets:
                packet = packets[in_packet_name]
                word_indices = packet['word']

                if in_word_index in word_indices:
                    word = word_indices[in_word_index]

                    if in_word_variant_name in word:
                        word_variant = word[in_word_variant_name]

                        if in_field_name in word_variant:
                            field = word_variant[in_field_name]
                            bits = field['bits']
                            assert bits != ''

                            list += [(gfx_version_to_int(gfx_version_name),
                                      engine_to_int(engine_name), bits)]

    return get_conflict_suffix(list, in_gfx_version_name, in_engine_name, in_field_bits)


# Return (suffix, comment) for the given field value.
# Disambiguation between gfx versions and engines is not implemented.
def get_value_conflict_suffix(gfx_versions, in_gfx_version_name, in_engine_name, in_packet_name, in_word_index,
                              in_word_variant_name, in_field_name, in_value_name, in_value_int):
    for gfx_version_name, engines in gfx_versions.items():
        for engine_name, packets in engines.items():
            if in_packet_name in packets:
                packet = packets[in_packet_name]
                enums = packet['enum']
                word_indices = packet['word']

                if in_word_index in word_indices:
                    word = word_indices[in_word_index]

                    if in_word_variant_name in word:
                        word_variant = word[in_word_variant_name]

                        # Conflicts between values of the same word are also possible.
                        for field_name, field in word_variant.items():
                            if field_name in enums:
                                enum = enums[field_name]

                                if in_value_name in enum:
                                    value = enum[in_value_name]
                                    value_int = value['value']

                                    if value['value'] != in_value_int:
                                        return ('_%s' % in_field_name, ' /* only %s */' % in_field_name)
    return ('', '')


def get_field_bits(field):
    bits_str = field['bits'] # Form: $last:$first or $first

    if ':' in bits_str:
        left, right = bits_str.split(':', 1)
        last_bit, first_bit = int(left), int(right)
    else:
        first_bit = int(bits_str)
        last_bit = first_bit + 1

    return (first_bit, last_bit)


def print2(s1, s2):
    print(s1.ljust(80) + s2)


def main():
    assert len(sys.argv) % 2 == 0 # argv = executable, N*2 input files, gfx$VERSION
    num_gfx_versions = (len(sys.argv) - 2) // 2
    assert num_gfx_versions > 0

    for i in range(1, len(sys.argv)):
        assert 'gfx' in sys.argv[i]

    gfx_version_param = sys.argv[-1]
    gfx_versions = {}
    gfx_opcodes = {}

    re_gfx_number = re.compile(r"gfx(\d+)")
    re_opcode = re.compile(r"^\s*IT_(?P<name>\w+)\s*=\s*(?P<hex>0x[\da-fA-F]+),*$")

    for i in range(num_gfx_versions):
        packet_filename = sys.argv[1 + i * 2]
        opcode_filename = sys.argv[1 + i * 2 + 1]

        assert (gfx_version_param in packet_filename) == (gfx_version_param in opcode_filename)

        file_gfx_version = 'gfx' + re_gfx_number.search(packet_filename).group(1)

        # Load the packet file
        engines = json.load(open(packet_filename, 'r', encoding='utf-8'))['pm4_packets']
        assert 'mes' not in engines, 'Removed "mes" from the json file.'
        gfx_versions[file_gfx_version] = engines

        # Load the opcode file
        opcode_file = open(opcode_filename, 'r', encoding='utf-8')
        opcodes = {}

        for line in opcode_file:
            match = re_opcode.match(line)
            if match:
                opcodes[match['name']] = int(match['hex'], 16)

        gfx_opcodes[file_gfx_version] = opcodes

    print(
"""/* This file is automatically generated. DO NOT EDIT.
 *
 * Copyright 2026 Advanced Micro Devices, Inc.
 * SPDX-License-Identifier: MIT
 */

/* Packets for each engine are defined separately.
 *
 * Engines:
 * - PFP = gfx queue engine 1
 * - MEG = gfx queue engine 2
 * - MEC = compute queue engine
 *
 * Packet field definition format: [SGC]_$OPCODE_$WORD_$FIELD
 *   Prefix meaning:
 *     S_... = set field
 *     G_... = get field
 *     C_... = clear field
 *   OPCODE = hexadecimal opcode number of the PKT3_* definition
 *   WORD = word index within the packet (0=header, 1=word1, 2=word2, ...)
 *   FIELD = the field within the word
 *
 *   E.g.: S_46_1_EVENT_TYPE
 *
 * If a definition contains GFX$version, it means the definition is only valid since that gfx version.
 * If a definition contains PFP, MEG, or MEC, it means the definition is only valid for those engines.
 * The comment after definitions, if present, lists gfx versions and engines where the definition is valid.
 */""")

    # Validate that the opcodes have defined packets
    for packet_name in gfx_opcodes[gfx_version_param].keys():
        if 'RESERVED' in packet_name:
            continue

        found = False
        for packets in gfx_versions[gfx_version_param].values():
            if packet_name in packets:
                found = True
                break

        if not found:
            print('/* NOTE: %s %s has opcode definition but no packet definition */' % (gfx_version_param, packet_name))

    # Validate that defined packets have opcodes
    for engine_name, packets in gfx_versions[gfx_version_param].items():
        for packet_name, packet in packets.items():
            if packet_name not in gfx_opcodes[gfx_version_param]:
                print('/* NOTE: %s %s has packet definition but no opcode definition */' % (gfx_version_param, packet_name))

    # Iterate over the dictionary.
    for engine_name, packets in gfx_versions[gfx_version_param].items():
        for packet_name, packet in packets.items():
            if packet_name not in gfx_opcodes[gfx_version_param]:
                continue

            opcode = gfx_opcodes[gfx_version_param][packet_name]
            opcode_suffix, opcode_comment = get_packet_conflict_suffix(gfx_opcodes, gfx_version_param, packet_name, opcode)

            print('')
            print2('#define PKT3_%s%s' % (packet_name, opcode_suffix.upper()),
                   '0x%X%s' % (opcode, opcode_comment))

            enums = packet['enum'] if 'enum' in packet else {}

            for word_index, word in packet['word'].items():
                if int(word_index) == 1:
                    continue # it's the packet header.

                for word_variant_name, word_variant in word.items():
                    if len(word.items()) > 1:
                        variant_letter = word_variant_name.upper()
                        variant_str = ' variant %s' % variant_letter
                    else:
                        variant_letter = ''
                        variant_str = ''

                    word_comment = ('%s PKT3_%s word %d%s' %
                                    (engine_name.upper(), packet_name, int(word_index) - 1, variant_str))

                    # If the number of fields is 1...
                    if len(word_variant) == 1:
                        field_name, field = next(iter(word_variant.items()))
                        first_bit, last_bit = get_field_bits(field)

                        if first_bit == 0 and last_bit == 31:
                            if field_name.startswith('reserved') or field_name.startswith('dummy'):
                                print('/* %s must be 0 */' % word_comment)
                            else:
                                print('/* %s is: %s (32 bits) */' % (word_comment, field_name.upper()))
                            continue

                    print('/* %s fields: */' % word_comment)

                    for field_name, field in word_variant.items():
                        if field_name.startswith('reserved'):
                            continue

                        field_suffix, field_comment = (
                            get_field_conflict_suffix(gfx_versions, gfx_version_param, engine_name, packet_name, word_index,
                                                      word_variant_name, field_name, field['bits']))
                        mangled_prefix = '%02X_%d%s' % (opcode, int(word_index) - 1, variant_letter)
                        mangled_name = '%s_%s%s' % (mangled_prefix, field_name.upper(), field_suffix.upper())

                        first_bit, last_bit = get_field_bits(field)
                        num_bits = last_bit - first_bit + 1
                        bitmask = (1 << num_bits) - 1
                        clear_mask = (bitmask << first_bit) ^ 0xffffffff

                        assert num_bits < 32
                        encode_field = '(((x) & 0x%x) << %d)' % (bitmask, first_bit)
                        decode_field = '(((x) >> %d) & 0x%x)' % (first_bit, bitmask)
                        clear_field = '0x%08X' % clear_mask

                        print2('#define   S_%s(x)' % mangled_name, encode_field + field_comment)
                        print2('#define   G_%s(x)' % mangled_name, decode_field)
                        print2('#define   C_%s' % mangled_name, clear_field)

                        if field_name in enums:
                            for value_name, value in enums[field_name].items():
                                if value_name.startswith('reserved'):
                                    continue

                                value_int = value['value']
                                value_suffix, value_comment = (
                                    get_value_conflict_suffix(gfx_versions, gfx_version_param, engine_name, packet_name, word_index,
                                                              word_variant_name, field_name, value_name, value_int))

                                print2('#define     V_%s_%s%s' % (mangled_prefix, value_name.upper(), value_suffix.upper()),
                                       str(value_int) + value_comment)


if __name__ == "__main__":
    main()
