Compilation and Installation Using Meson
========================================

1. Introduction
---------------

For general information about Meson see the `Meson
website <https://mesonbuild.com/>`__.

.. note::

   If your distribution doesn't have something recent enough in its
   repositories, you can `try the methods suggested here
   <https://mesonbuild.com/Getting-meson.html>`__ to install the
   current version of Meson.

The Meson build of Mesa is tested on Linux, macOS, Windows, Cygwin,
Haiku, FreeBSD, DragonflyBSD, NetBSD, and should work on OpenBSD.

Unix-like OSes
^^^^^^^^^^^^^^

If Meson is not already installed on your system, you can typically
install it with your package installer. For example:

.. code-block:: sh

   sudo apt-get install meson   # Ubuntu

or

.. code-block:: sh

   sudo dnf install meson   # Fedora

Some older versions of Meson do not check that they are too old and will
error out in odd ways.

You'll also need `Ninja <https://ninja-build.org/>`__. If it's not
already installed, use apt-get or dnf to install the *ninja-build*
package.

Dependencies
++++++++++++

Following are the dependencies you would need to install on linux to build and install mesa main. You can install these packages using your linux distibutions' package manager.
On Debian, Ubuntu and similar, ``sudo apt-get build-dep mesa`` installs
most of these with just one command. On Fedora and similar, ``sudo dnf
builddep mesa`` does the same.

.. note::
   All these dependencies are for latest Linux distributions and is tested on Ubuntu 24.xx only for now.

   Also note, some packages below might not be available in your OS with the exact name, in such case you can search for it and install the distribution specific one.

   For some packages (eg: libclc etc), you will need the full content of the packages, so make sure to also install ``-dev``/``-devel``/``-headers``/etc. packages (if available) on distributions that split the files into multiple packages.

1. glslang-tools
2. python3-pyyaml
3. python3-mako
4. libdrm (This will get libdrm for intel, amd, qualcomm, nvidia, etc. If you are building a specific driver out of these, you can install only that specific libdrm)
5. libclc-<version>
6. llvm-<version>
7. libllvmspirvlib-<version>
8. libclang-<version>
9. byacc (or) bison
10. flex

.. note::
   You should make sure that all the llvm related packages (libclc, libclc-dev, llvm, libllvmspirvlib, libclang) are of the same version. You can go with the latest version available on your OS if you are not aware of which version to select.

wayland specific:

1. libwayland
2. libwayland-egl-backend

x11 specific:

1. libx11
2. libxext
3. libxfixes
4. libxcb-glx
5. libxcb-shm
6. libx11-xcb
7. libxcb-dri2
8. libxcb-dri3
9. libxcb-present
10. libxshmfence
11. libxxf86vm
12. libxrandr

for intel vulkan ray-tracing:

1. python3-ply

radeon specific:

1. libelf

nouveau/rusticl specific:

1. rustc
2. rustfmt
3. bindgen
4. cbindgen

Windows
^^^^^^^

You will need to install Python 3 and Meson as a module using pip. This
is because we use Python for generating code, and rely on external
modules (Mako). You also need pkg-config (a hard dependency of Meson),
Flex, and Bison. The easiest way to install everything you need is with
`Chocolatey <https://chocolatey.org/>`__.

.. code-block:: sh

   choco install python3 winflexbison pkgconfiglite

You can even use Chocolatey to install MinGW and Ninja (Ninja can be
used with MSVC as well)

.. code-block:: sh

   choco install ninja mingw

Then install Meson using pip

.. code-block:: sh

   py -3 -m pip install meson packaging mako

You may need to add the Python 3 scripts directory to your path for
Meson.

2. Basic Usage
--------------

The Meson program is used to configure the source directory and
generates either a Ninja build file or Visual Studio® build files. The
latter must be enabled via the ``--backend`` switch, as Ninja is the
default backend on all operating systems.

Meson only supports out-of-tree builds, and must be passed a directory
to put built and generated sources into. We'll call that directory
"build" here. It's recommended to create a `separate build
directory <https://mesonbuild.com/Using-multiple-build-directories.html>`__
for each configuration you might want to use.

Basic configuration is done with:

.. code-block:: sh

   meson setup build/

This will create the build directory. If any dependencies are missing,
you can install them, or try to remove the dependency with a Meson
configuration option (see below). Meson will print a summary of the
build options at the end.

To review the options which Meson chose, run:

.. code-block:: sh

   meson configure build/

Recent version of Meson can print the available options and their
default values by running ``meson configure`` in the source directory.
If your Meson version is too old, you can always look in the
`meson.options <https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/meson.options>`__
file at the root of the project.

With additional arguments ``meson configure`` can be used to change
options for a previously configured build directory. All options passed
to this command are in the form ``-D "option"="value"``. For example:

.. code-block:: sh

   meson configure build/ -Dprefix=/tmp/install -Dglx=true

Note that options taking lists (such as ``platforms``) are `a bit more
complicated <https://mesonbuild.com/Build-options.html#using-build-options>`__,
but the simplest form compatible with Mesa options is to use a comma to
separate values (``-D platforms=drm,wayland``) and brackets to represent
an empty list (``-D platforms=[]``).

Once you've run the initial ``meson`` command successfully you can use
your configured backend to build the project in your build directory:

.. code-block:: sh

   ninja -C build/

The next step is to install the Mesa libraries, drivers, etc. This also
finishes up some final steps of the build process (such as creating
symbolic links for drivers). To install:

.. code-block:: sh

   ninja -C build/ install

After installation, you can check if the installation happened properly or not by running the command:

.. code-block:: sh

   glxinfo | grep OpenGL

If the installation succeeded, you should see the Mesa devel version and also the commit hash of the latest commit.

In case you don't see the devel version, you can run

.. code-block:: sh

   sudo ldconfig

Windows specific instructions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On Windows you have a couple of choices for compilers. If you installed
MinGW with Chocolatey and want to use Ninja you should be able to open
any shell and follow the instructions above. If you want to you MSVC,
clang-cl, or ICL (the Intel Compiler), read on.

Both ICL and MSVC come with shell environments, the easiest way to use
Meson with these it to open a shell. For clang-cl you will need to open
an MSVC shell, and then override the compilers, either using a `native
file <https://mesonbuild.com/Native-environments.html>`__, or with the
CC and CXX environment variables.

All of these compilers are tested and work with Ninja, but if you want
Visual Studio integration or you just like msbuild, passing
``--backend=vs`` to Meson will generate a Visual Studio solution.

3. Advanced Usage
-----------------

Installation Location
^^^^^^^^^^^^^^^^^^^^^

Meson default to installing :file:`libGL.so` in your system's main
:file:`lib/` directory and DRI drivers to a :file:`dri/` subdirectory.

Developers will often want to install Mesa to a testing directory rather
than the system library directory. This can be done with the --prefix
option. For example:

.. code-block:: sh

   meson --prefix="${PWD}/build/install" build/

will put the final libraries and drivers into the build/install/
directory. Then you can set LD_LIBRARY_PATH to that location to run/test
the driver.

Meson also honors ``DESTDIR`` for installs.

Compiler Options
^^^^^^^^^^^^^^^^

Meson supports the common CFLAGS, CXXFLAGS, etc. environment variables
but their use is discouraged because of the many caveats in using them.

Instead, it is recommended to use ``-D${lang}_args`` and
``-D${lang}_link_args``. Among the benefits of these options is that
they are guaranteed to persist across rebuilds and reconfigurations.

This example sets -fmax-errors for compiling C sources and -DMAGIC=123
for C++ sources:

.. code-block:: sh

   meson setup builddir/ -Dc_args=-fmax-errors=10 -Dcpp_args=-DMAGIC=123

Compiler Specification
^^^^^^^^^^^^^^^^^^^^^^

Meson supports the standard CC and CXX environment variables for
changing the default compiler. Note that Meson does not allow changing
the compilers in a configured build directory so you will need to create
a new build dir for a different compiler.

This is an example of specifying the Clang compilers and cleaning the
build directory before reconfiguring with an extra C option:

.. code-block:: sh

   CC=clang CXX=clang++ meson setup build-clang
   ninja -C build-clang
   ninja -C build-clang clean
   meson configure build -Dc_args="-Wno-typedef-redefinition"
   ninja -C build-clang

The default compilers depends on your operating system. Meson supports
most of the popular compilers, a complete list is available
`here <https://mesonbuild.com/Reference-tables.html#compiler-ids>`__.

LLVM
^^^^

Meson includes upstream logic to wrap llvm-config using its standard
dependency interface.

Meson can use CMake to find LLVM. But due to the way LLVM implements its
CMake finder it will only find static libraries, it will never find
:file:`libllvm.so`. There is also a ``-Dcmake_module_path`` option,
which points to the root of an alternative installation (the prefix).
For example:

.. code-block:: sh

   meson setup builddir -Dcmake_module_path=/home/user/mycmake/prefix

As of Meson 0.49.0 Meson also has the concept of a `"native
file" <https://mesonbuild.com/Native-environments.html>`__, these files
provide information about the native build environment (as opposed to a
cross build environment). They are INI formatted and can override where
to find llvm-config:

.. code-block:: ini
   :caption: custom-llvm.ini

   [binaries]
   llvm-config = '/usr/local/bin/llvm/llvm-config'

Then configure Meson:

.. code-block:: sh

   meson setup builddir/ --native-file custom-llvm.ini

For selecting llvm-config for cross compiling a `"cross
file" <https://mesonbuild.com/Cross-compilation.html#defining-the-environment>`__
should be used. It uses the same format as the native file above:

.. code-block:: ini
   :caption: cross-llvm.ini

   [binaries]
   ...
   llvm-config = '/usr/lib/llvm-config-32'
   cmake = '/usr/bin/cmake-for-my-arch'

Obviously, only CMake or llvm-config is required.

Then configure Meson:

.. code-block:: sh

   meson setup builddir/ --cross-file cross-llvm.ini

See the :ref:`Cross Compilation <cross-compilation>` section for more
information.

On Windows (and in other cases), using llvm-config or CMake may be
either undesirable or impossible. Meson's solution for this is a
`wrap <https://mesonbuild.com/Wrap-dependency-system-manual.html>`__, in
this case a "binary wrap". Follow the steps below:

-  Install the binaries and headers into the
   ``$mesa_src/subprojects/llvm``
-  Add a :file:`meson.build` file to that directory (more on that later)

The wrap file must define the following:

-  ``dep_llvm``: a ``declare_dependency()`` object with
   include_directories, dependencies, and version set)

It may also define:

-  ``irbuilder_h``: a ``files()`` object pointing to llvm/IR/IRBuilder.h
-  ``has_rtti``: a ``bool`` that declares whether LLVM was built with
   RTTI. Defaults to true

such a :file:`meson.build` file might look like:

::

   project('llvm', ['cpp'])

   cpp = meson.get_compiler('cpp')

   _deps = []
   _search = join_paths(meson.current_source_dir(), 'lib')
   foreach d : ['libLLVMCodeGen', 'libLLVMScalarOpts', 'libLLVMAnalysis',
                'libLLVMTransformUtils', 'libLLVMCore', 'libLLVMX86CodeGen',
                'libLLVMSelectionDAG', 'libLLVMipo', 'libLLVMAsmPrinter',
                'libLLVMInstCombine', 'libLLVMInstrumentation', 'libLLVMMC',
                'libLLVMGlobalISel', 'libLLVMObjectYAML', 'libLLVMDebugInfoPDB',
                'libLLVMVectorize', 'libLLVMPasses', 'libLLVMSupport',
                'libLLVMLTO', 'libLLVMObject', 'libLLVMDebugInfoCodeView',
                'libLLVMDebugInfoDWARF', 'libLLVMOrcJIT', 'libLLVMProfileData',
                'libLLVMObjCARCOpts', 'libLLVMBitReader', 'libLLVMCoroutines',
                'libLLVMBitWriter', 'libLLVMRuntimeDyld', 'libLLVMMIRParser',
                'libLLVMX86Desc', 'libLLVMAsmParser', 'libLLVMTableGen',
                'libLLVMFuzzMutate', 'libLLVMLinker', 'libLLVMMCParser',
                'libLLVMExecutionEngine', 'libLLVMCoverage', 'libLLVMInterpreter',
                'libLLVMTarget', 'libLLVMX86AsmParser', 'libLLVMSymbolize',
                'libLLVMDebugInfoMSF', 'libLLVMMCJIT', 'libLLVMXRay',
                'libLLVMX86AsmPrinter', 'libLLVMX86Disassembler',
                'libLLVMMCDisassembler', 'libLLVMOption', 'libLLVMIRReader',
                'libLLVMLibDriver', 'libLLVMDlltoolDriver', 'libLLVMDemangle',
                'libLLVMBinaryFormat', 'libLLVMLineEditor',
                'libLLVMWindowsManifest', 'libLLVMX86Info', 'libLLVMX86Utils']
     _deps += cpp.find_library(d, dirs : _search)
   endforeach

   dep_llvm = declare_dependency(
     include_directories : include_directories('include'),
     dependencies : _deps,
     version : '6.0.0',
   )

   has_rtti = false
   irbuilder_h = files('include/llvm/IR/IRBuilder.h')

It is very important that version is defined and is accurate, if it is
not, workarounds for the wrong version of LLVM might be used resulting
in build failures.

``PKG_CONFIG_PATH``
^^^^^^^^^^^^^^^^^^^

The ``pkg-config`` utility is a hard requirement for configuring and
building Mesa on Unix-like systems. It is used to search for external
libraries on the system. This environment variable is used to control
the search path for ``pkg-config``. For instance, setting
``PKG_CONFIG_PATH=/usr/X11R6/lib/pkgconfig`` will search for package
metadata in ``/usr/X11R6`` before the standard directories.

Options
^^^^^^^

One of the oddities of Meson is that some options are different when
passed to :program:`meson` than to ``meson configure``. These options are
passed as --option=foo to :program:`meson`, but -Doption=foo to
``meson configure``. Mesa defined options are always passed as
-Doption=foo.

For those coming from Autotools be aware of the following:

``--buildtype/-Dbuildtype``
   This option will set the compiler debug/optimization levels to aid
   debugging the Mesa libraries.

   Note that in Meson this defaults to ``debugoptimized``, and not
   setting it to ``release`` will yield non-optimal performance and
   binary size. Not using ``debug`` may interfere with debugging as some
   code and validation will be optimized away.

   For those wishing to pass their own optimization flags, use the
   ``plain`` buildtype, which causes Meson to inject no additional
   compiler arguments, only those in the C/CXXFLAGS and those that mesa
   itself defines.

``-Db_ndebug``
   This option controls assertions in Meson projects. When set to
   ``false`` (the default) assertions are enabled, when set to true they
   are disabled. This is unrelated to the ``buildtype``; setting the
   latter to ``release`` will not turn off assertions.

.. _cross-compilation:

4. Cross-compilation and 32-bit builds
--------------------------------------

`Meson supports
cross-compilation <https://mesonbuild.com/Cross-compilation.html>`__ by
specifying a number of binary paths and settings in a file and passing
this file to ``meson`` or ``meson configure`` with the ``--cross-file``
parameter.

This file can live at any location, but you can use the bare filename
(without the folder path) if you put it in
:file:`$XDG_DATA_HOME/meson/cross` or :file:`~/.local/share/meson/cross`

Below are a few example of cross files, but keep in mind that you will
likely have to alter them for your system.

Those running on Arch Linux can use the AUR-maintained packages for some
of those, as they'll have the right values for your system:

-  `meson-cross-x86-linux-gnu <https://aur.archlinux.org/packages/meson-cross-x86-linux-gnu>`__
-  `meson-cross-aarch64-linux-gnu <https://github.com/dcbaker/archlinux-meson-cross-aarch64-linux-gnu>`__

Cross-compilation requires cross-compiled versions of the same build
dependencies listed at the top of this page.

On Debian, Ubuntu and similar, the command ``sudo apt-rdepends
--build-depends --follow=DEPENDS mesa`` provides a complete and
up-to-date list of all build dependencies. Append the ``:i386`` suffix
(or your desired architecture) to package names and install the ones you
need like this: ``sudo apt install libwayland-dev:i386 libelf-dev:i386
...``

On Fedora and similar, try ``sudo setarch i686 dnf builddep mesa``. If
that fails, ``sudo dnf builddep mesa`` prints all native dependencies
even when they are already installed. Replace the ``.x86_64`` suffix
with ``.686`` (or your desired architecture) and install the ones you
need.

You do not need a cross-compiled version of the dependencies like
``python3``, ``bison``, ``flex``, ``bindgen``,...  that are *used only at
build time*. Some unnecessary ones may even confuse your system
configuration. Also, remember that some dependencies are optional
depending on how you configure your ``meson setup ...`` command - this
is not specific to cross-compilation; see details above. So you may want
to proceed with trial-and-error and install only cross-compiled packages
needed to fix build error messages.

32-bit build on x86 linux:

.. code-block:: ini

   [binaries]
   c = '/usr/bin/gcc'
   cpp = '/usr/bin/g++'
   # ccache is not automatically used when specifying [binaries].
   # To accelerate cross-compilation as much as native compilation:
   # c =   ['ccache', 'gcc']
   # cpp = ['ccache', 'g++']
   ar = '/usr/bin/gcc-ar'
   strip = '/usr/bin/strip'
   llvm-config = '/usr/bin/llvm-config32'
   pkg-config = '/usr/bin/pkg-config-32'
   # As of version 40, Fedora uses a full target platform prefix for
   # pkg-config instead, like the ARM and Windows examples below:
   # pkg-config = 'i686-redhat-linux-gnu-pkg-config'

   [built-in options]
   c_args = ['-m32']
   c_link_args = ['-m32']
   cpp_args = ['-m32']
   cpp_link_args = ['-m32']

   [host_machine]
   system = 'linux'
   cpu_family = 'x86'
   cpu = 'i686'
   endian = 'little'

64-bit build on ARM linux:

.. code-block:: ini

   [binaries]
   c = '/usr/bin/aarch64-linux-gnu-gcc'
   cpp = '/usr/bin/aarch64-linux-gnu-g++'
   ar = '/usr/bin/aarch64-linux-gnu-gcc-ar'
   strip = '/usr/bin/aarch64-linux-gnu-strip'
   pkg-config = '/usr/bin/aarch64-linux-gnu-pkg-config'
   exe_wrapper = '/usr/bin/qemu-aarch64-static'

   [host_machine]
   system = 'linux'
   cpu_family = 'aarch64'
   cpu = 'aarch64'
   endian = 'little'

64-bit build on x86 Windows:

.. code-block:: ini

   [binaries]
   c = '/usr/bin/x86_64-w64-mingw32-gcc'
   cpp = '/usr/bin/x86_64-w64-mingw32-g++'
   ar = '/usr/bin/x86_64-w64-mingw32-ar'
   strip = '/usr/bin/x86_64-w64-mingw32-strip'
   pkg-config = '/usr/bin/x86_64-w64-mingw32-pkg-config'
   exe_wrapper = 'wine'

   [host_machine]
   system = 'windows'
   cpu_family = 'x86_64'
   cpu = 'i686'
   endian = 'little'
