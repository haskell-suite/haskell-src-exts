Haskell Source Extensions
=========================

haskell-src-exts is a package for handling and manipulating Haskell source
code. It is a descendant of the haskell-src package that is part of the standard
libraries, but extends this to support a number of syntactic
extensions, e.g. MPTCs, fundeps, GADTs, TH etc. The aim is to support all
extensions recognized by the community, as determined by what is implemented
in compilers and tools.

Apart from the more standard extensions supported by e.g. GHC,
haskell-src-exts provides support for HaRP (Haskell Regular Patterns)
and HSX (Haskell Source with XML) syntax.

Package structure
-----------------

The modules that comprise haskell-src-exts all reside in the hierarchic
namespace Language.Haskell.Exts. Notable exposed modules include:

* `Language.Haskell.Exts` - Imports and re-exports all the below,
  and also defines some functions that combine functionality from several
  modules.
* `Language.Haskell.Exts.Syntax` - The abstract syntax tree
  that the other modules work on.
* `Language.Haskell.Exts.Build` - Combinators for building
  abstract syntax.
* `Language.Haskell.Exts.Parser` - Functions for parsing Haskell
  source code into an abstract syntax representation.


License
-------

The haskell-src-exts Package is distributed under a derived BSD-style license. It
derives from several sources, all of which are distributable under
BSD-style or compatible licenses. See the file LICENSE for the complete
license text.


Maintenance
--------------

Dan Burton is currently keeping haskell-src-exts on life support.
If you are interested in more actively making improvements to this package,
please make your interests known.

You might want to try [ghc-lib-parser](http://hackage.haskell.org/package/ghc-lib-parser) instead.
