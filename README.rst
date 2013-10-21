=============
Baardskeerder
=============

This fork is intended for a variant of Baardskeerder to run on 
Mirage_. The persistent back-end
is intended to be a Mirage OS.Devices.blkif and an API facade is intended to 
provide a mutable key-value API analogous to OS.Devices.kv_ro. Status is just
starting; some delta are likely to be from this variant_.

Baardskeerder is a so-called Copy-on-Write B-tree_ (aka Append-only B-tree),
a fully-persistent datastructure which can be implemented using nothing but
append writes towards the storage medium on which the data is stored.

A CoW B-tree implements a fully-persistent_ B-tree datastructure by copying
paths from leaf to root whenever a given key is added (or altered) inside the
tree.

This is a technology preview, so keep this in mind whenever thinking about
using this in a *real-world* application: the codebase is still in flux, the
on-disk database format will most certainly change (without backwards
compatibility) before the first version is released,...

.. _B-tree: http://en.wikipedia.org/wiki/B-tree
.. _fully-persistent: http://en.wikipedia.org/wiki/Persistent_data_structure
.. _Mirage: https://github.com/mirage/mirage-platform
.. _variant: https://github.com/djs55/mirage/commits/baardskeerder/lib/btree

Getting Started
===============
Baardskeerder uses several features only provided by the Linux kernel, so it
can't (for now) be used on other platforms. Some features require a very recent
(>= 3.0) kernel and require specific file-system support, currently only
provided by `ext4`, `XFS` and `OCFS2`.

It's written in OCaml_, so you'll need a recent (>= 3.12) OCaml compiler on your system.
The build system uses `ocamlbuild` and `ocamlfind`.


Finally, a working C compiler and up-to-date system header files are required.

To build the library, test executable and benchmark tool, execute `make` in the
`src` tree. Run `make install` to install the library on your system.

To render the documentation, you'll need Sphinx_ and its dependencies installed.
Execute `make html` in the `doc` tree to generate HTML output. Execute
`make help` to see a list of other output formats available.

.. _OCaml: http://caml.inria.fr/ocaml/
.. _Sphinx: http://sphinx.pocoo.org/

Warning: the documentation is out of date, and updating it is currently low priority.
If you want to know how it works, take a look at our blog: `Incubaid Research Blog`_ 

.. _Incubaid Research Blog: http://blog.incubaid.com


License
=======
Baardskeerder is available under the LGPL-3 license. See COPYING_ for more
information.

.. _COPYING: https://raw.github.com/Incubaid/baardskeerder/master/COPYING
