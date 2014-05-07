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
namespace Language.Haskell.Exts, or its more feature-rich sibling
Language.Haskell.Exts.Annotated. Notable exposed modules include:

* `Language.Haskell.Exts[.Annotated]` - Imports and re-exports all the below,
  and also defines some functions that combine functionality from several
  modules.
* `Language.Haskell.Exts[.Annotated].Syntax` - The abstract syntax tree
  that the other modules work on.
* `Language.Haskell.Exts[.Annotated].Build` - Combinators for building
  abstract syntax.
* `Language.Haskell.Exts[.Annotated].Parser` - Functions for parsing Haskell
  source code into an abstract syntax representation.

Non-comprehensive list of supported extensions
----------------------------------------------

* Multi-parameter type classes (MPTCs)
* Functional dependencies
* Associated types, type families
* Liberal class and instance heads
* Implicit parameters (ghc and hugs)
* Explicit kind signatures
* Pattern guards
* Generalized algebraic data types (GADTs)
* Template Haskell (TH)
* Universal and existential quantification (forall)
* Empty data type declarations
* Unboxed tuples `(# #)`
* Standalone deriving
* Regular patterns
* Haskell XML, HSX style
* Pragmas

Portability and dependencies
----------------------------

The package itself is Haskell98-compliant and should build with any
Haskell compiler.

License
-------

The haskell-src-exts Package is distributed under a derived BSD-style license. It
derives from several sources, all of which are distributable under
BSD-style or compatible licenses. See the file LICENSE for the complete
license text.
