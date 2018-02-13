========================
LLVM 6.0.0 Release Notes
========================

.. contents::
    :local:

.. warning::
   These are in-progress notes for the upcoming LLVM 6 release.
   Release notes for previous releases can be found on
   `the Download Page <http://releases.llvm.org/download.html>`_.


Introduction
============

This document contains the release notes for the LLVM Compiler Infrastructure,
release 6.0.0.  Here we describe the status of LLVM, including major improvements
from the previous release, improvements in various subprojects of LLVM, and
some of the current users of the code.  All LLVM releases may be downloaded
from the `LLVM releases web site <http://llvm.org/releases/>`_.

For more information about LLVM, including information about the latest
release, please check out the `main LLVM web site <http://llvm.org/>`_.  If you
have questions or comments, the `LLVM Developer's Mailing List
<http://lists.llvm.org/mailman/listinfo/llvm-dev>`_ is a good place to send
them.

Note that if you are reading this file from a Subversion checkout or the main
LLVM web page, this document applies to the *next* release, not the current
one.  To see the release notes for a specific release, please see the `releases
page <http://llvm.org/releases/>`_.

Non-comprehensive list of changes in this release
=================================================
.. NOTE
   For small 1-3 sentence descriptions, just add an entry at the end of
   this list. If your description won't fit comfortably in one bullet
   point (e.g. maybe you would like to give an example of the
   functionality, or simply have a lot to talk about), see the `NOTE` below
   for adding a new subsection.

* The ``Redirects`` argument of ``llvm::sys::ExecuteAndWait`` and
  ``llvm::sys::ExecuteNoWait`` was changed to an ``ArrayRef`` of optional
  ``StringRef``'s to make it safer and more convenient to use.

* The backend name was added to the Target Registry to allow run-time
  information to be fed back into TableGen. Out-of-tree targets will need to add
  the name used in the `def X : Target` definition to the call to
  `RegisterTarget`.

* The ``Debugify`` pass was added to ``opt`` to facilitate testing of debug
  info preservation. This pass attaches synthetic ``DILocations`` and
  ``DIVariables`` to the instructions in a ``Module``. The ``CheckDebugify``
  pass determines how much of the metadata is lost.

* Significantly improved quality of CodeView debug info for Windows.

* Note..

.. NOTE
   If you would like to document a larger change, then you can add a
   subsection about it right here. You can copy the following boilerplate
   and un-indent it (the indentation causes it to be inside this comment).

   Special New Feature
   -------------------

   Makes programs 10x faster by doing Special New Thing.

Changes to the LLVM IR
----------------------

Changes to the AArch64 Target
-----------------------------

During this release:

 * Enabled the new GlobalISel instruction selection framework by default at ``-O0``.

Changes to the ARM Target
-------------------------

During this release the ARM target has:

* Got support for enabling SjLj exception handling on platforms where it
  isn't the default.


Changes to the Hexagon Target
-----------------------------

* The Hexagon backend now supports V65 ISA.

* The ``-mhvx`` option now takes an optional value that specified the ISA
  version of the HVX coprocessor.  The available values are v60, v62 and v65.
  By default, the value is set to be the same as the CPU version.

* The compiler option ``-mhvx-double`` is deprecated and will be removed in
  the next release of the compiler. Programmers should use ``-mhvx-length``
  option to specify the desired vector length: ``-mhvx-length=64b`` for
  64-byte vectors and ``-mhvx-length=128b`` for 128-byte vectors. While the
  current default vector length is 64 bytes, users should always specify the
  length explicitly, since the default value may change in the future.

* The target feature ``hvx-double`` is deprecated and will be removed in the
  next release. LLVM IR generators should use target features ``hvx-length64b``
  and ``hvx-length128b`` to indicate the vector length. The length should
  always be specified when HVX code generation is enabled.


Changes to the MIPS Target
--------------------------

 During this release ...


Changes to the PowerPC Target
-----------------------------

 During this release ...

Changes to the SystemZ Target
-----------------------------

During this release the SystemZ target has:

* Added support for 128-bit atomic operations.

* Added support for the "o" constraint for inline asm statements.

Changes to the X86 Target
-------------------------

During this release ...

* Got support for enabling SjLj exception handling on platforms where it
  isn't the default.

Changes to the AMDGPU Target
-----------------------------

 During this release ...

Changes to the AVR Target
-----------------------------

 During this release ...

Changes to the OCaml bindings
-----------------------------

 During this release ...


Changes to the C API
--------------------

 During this release ...


External Open Source Projects Using LLVM 6
==========================================

JFS - JIT Fuzzing Solver
------------------------

`JFS <https://github.com/delcypher/jfs>`_ is an experimental constraint solver
designed to investigate using coverage guided fuzzing as an incomplete strategy
for solving boolean, BitVector, and floating-point constraints.
It is built on top of LLVM, Clang, LibFuzzer, and Z3.

The solver works by generating a C++ program where the reachability of an
`abort()` statement is equivalent to finding a satisfying assignment to the
constraints. This program is then compiled by Clang with `SanitizerCoverage
<https://releases.llvm.org/6.0.0/tools/clang/docs/SanitizerCoverage.html>`_
instrumentation and then fuzzed using :doc:`LibFuzzer <LibFuzzer>`.

Zig Programming Language
------------------------

`Zig <http://ziglang.org>`_  is an open-source programming language designed
for robustness, optimality, and clarity. It is intended to replace C. It
provides high level features such as Generics,
Compile Time Function Execution, and Partial Evaluation, yet exposes low level
LLVM IR features such as Aliases. Zig uses Clang to provide automatic
import of .h symbols - even inline functions and macros. Zig uses LLD combined
with lazily building compiler-rt to provide out-of-the-box cross-compiling for
all supported targets.

LDC - the LLVM-based D compiler
-------------------------------

`D <http://dlang.org>`_ is a language with C-like syntax and static typing. It
pragmatically combines efficiency, control, and modeling power, with safety and
programmer productivity. D supports powerful concepts like Compile-Time Function
Execution (CTFE) and Template Meta-Programming, provides an innovative approach
to concurrency and offers many classical paradigms.

`LDC <http://wiki.dlang.org/LDC>`_ uses the frontend from the reference compiler
combined with LLVM as backend to produce efficient native code. LDC targets
x86/x86_64 systems like Linux, OS X, FreeBSD and Windows and also Linux on ARM
and PowerPC (32/64 bit). Ports to other architectures like AArch64 and MIPS64
are underway.

Additional Information
======================

A wide variety of additional information is available on the `LLVM web page
<http://llvm.org/>`_, in particular in the `documentation
<http://llvm.org/docs/>`_ section.  The web page also contains versions of the
API documentation which is up-to-date with the Subversion version of the source
code.  You can access versions of these documents specific to this release by
going into the ``llvm/docs/`` directory in the LLVM tree.

If you have any questions or comments about LLVM, please feel free to contact
us via the `mailing lists <http://llvm.org/docs/#maillist>`_.
