# Copyright © 2019 Intel Corporation

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

vklayer_files = files(
  'screenshot.cpp',
  'screenshot_params.c',
)

vklayer_mesa_screenshot = shared_library(
  'VkLayer_MESA_screenshot',
  vklayer_files, sha1_h,
  c_args : [no_override_init_args],
  gnu_symbol_visibility : 'hidden',
  dependencies : [idep_vulkan_util, idep_mesautil, vulkan_wsi_deps, dep_dl, dependency('libpng')],
  include_directories : [inc_include, inc_src],
  link_args : cc.get_supported_link_arguments(['-Wl,-Bsymbolic-functions', '-Wl,-z,relro']),
  install : true
)

configure_file(
  configuration : {'library_path' : 'libVkLayer_MESA_screenshot.so'},
  input : 'VkLayer_MESA_screenshot.json.in',
  output : '@BASENAME@',
  install : true,
  install_dir : join_paths(get_option('datadir'), 'vulkan', 'explicit_layer.d'),
  install_tag : 'runtime',
)

configure_file(
  configuration : {'library_path' : vklayer_mesa_screenshot.full_path()},
  input : 'VkLayer_MESA_screenshot.json.in',
  output : '@PLAINNAME@_dev.json',
)
devenv.append('VK_ADD_LAYER_PATH', meson.current_build_dir())

install_data(
  'mesa-screenshot-control.py',
  install_dir : get_option('bindir'),
  install_mode : 'r-xr-xr-x',
)
