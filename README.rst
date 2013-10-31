=============
Baardskeerder
=============

This fork is intended for a variant of Baardskeerder to run on 
Mirage_. The persistent back-end
is intended to be a Mirage OS.Devices.blkif.

Status is implemented basic store over blkif in src/blkif.ml and ported flog0
log implementation to work with it. There is a page cache which is mainly 
LRU plus a couple of optimisations (first and last page). The store API
has been narrowed to remove next, append and with_fd. Performance currently
seems to be about 4-10 times slower using the Blkif store with a 
unix-simple-blkdev over a file than using the direct Lwt store.

Platform-independent code is now in the mirage-baardskeerder package while
Mirage-specific code (including the Blkif store) is in the .mirage sub-
package and Unix-specific code (including the Flog logger and Sync and
Lwt stores) are in the .unix sub-package. It should be possible to make
a platform-independent (or mirage-specific) variant of the Flog logger
but I haven't yet done do.


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
