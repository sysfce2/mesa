# Copyright © 2025 Google
# SPDX-License-Identifier: MIT

project(
  'rustix',
  'rust',
  version : '1.0.7',
  license : 'Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT',
)

errno = subproject('errno-0.3-rs').get_variable('lib')
libc = subproject('libc-0.2-rs').get_variable('lib')
bitflags = subproject('bitflags-2-rs').get_variable('lib')

rustix_args = []
if host_machine.system() == 'linux' or host_machine.system() == 'android'
  rustix_args += [
  '--cfg', 'linux_like',
  '--cfg', 'linux_kernel',
  '--cfg', 'libc',
  '--cfg', 'feature="use-libc"',
  '--cfg', 'feature="std"',
  '--cfg', 'feature="alloc"',
  '--cfg', 'feature="event"',
  '--cfg', 'feature="fs"',
  '--cfg', 'feature="mm"',
  '--cfg', 'feature="net"',
  '--cfg', 'feature="param"',
  '--cfg', 'feature="pipe"',
]
endif

lib = static_library(
  'rustix',
  'src/lib.rs',
  override_options : ['rust_std=2021', 'build.rust_std=2021'],
  link_with : [errno, libc, bitflags],
  rust_abi : 'rust',
  native : true,
  rust_args: rustix_args,
)

dep_rustix = declare_dependency(
  link_with : [lib, errno, libc, bitflags]
)
