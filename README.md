# Vernacular

Vernacular is a build and module system for languages that compile to Common Lisp. Vernacular handles locating files, compiling files into fasls, tracking dependencies and rebuilding, and export and import between your new language, Lisp, and any other language Vernacular supports.

Vernacular builds on [Overlord][] and is heavily inspired by [Racket][].

# Examples

Here are some example languages:

1. [Bosom Serpent][]. Shows how to wrap a foreign runtime (Python, using [burgled-batteries][]) as an Vernacular module.

2. [vernacular/demo/js](demo/js.lisp). A simple language built on [CL-JavaScript][]. Shows how to convert a pre-existing CL language implementation to work with Vernacular.

3. [cl-yesql][]. Lisp port of Clojure’s [yesql][]: lets you write SQL queries in separate SQL files, using special comments to guide conversion into Lisp functions. Includes a non-trivial parser.

4. [Core Lisp][]. A hygiene-compatible implementation of the Lisp dialect [ISLISP][] (itself a conceptual subset of Common Lisp). Shows how to use Vernacular to build “language towers.”

# Using a Vernacular language

Let’s use use CL-Yesql as an example.

In your system definition, add `"cl-yesql/sqlite"` as a dependency.

Create a file named `fact.sql` in your system, and add the following:

    #lang cl-yesql/sqlite

    -- name: create-facts-table @execute
    CREATE TABLE fact (subject text, fact text)

    -- name: add-fact @execute
    INSERT INTO fact (subject, fact) VALUES (?, ?)

    -- name: facts-about @column
    SELECT fact FROM fact WHERE subject = ?

In a Lisp file in your system, add:

    (vernacular:import facts
      :from "fact.sql"
      :binding :all-functions)

You can then do:

    (sqlite:with-open-database (db ":memory:")
        (create-facts-table db)
        (add-fact db "Lisp" "Lisp is a programmable programming language.")
        (add-fact db "Lisp" "Lisp is fun")
        (facts-about db "Lisp"))
    ;; => '("Lisp is a programmable programming language." "Lisp is fun")

Two cavets. Vernacular needs to know what system your package belongs to. If your package has a different name from your system (why?), you may need to tell Vernacular what system it belongs to. In the package, evaluate:

    (overlord:set-package-system :my-system)

Also, note that import paths are given relative to the base of the system, not the file with the import form. Whether you are importing from `all-in-one.lisp` at the base of your project, or  `deeply/nested/component/impl.lisp`, the path does not change.

## Specifying languages

The language of a file can be specified in three ways.

The preferred way is to use a special first line:

    #lang my-lang
    ....

This is called (following Racket) a *hash lang*. A hash lang takes precedence over all other ways of specifying a language.

Sometimes, however, you will be re-using an existing syntax. This lets you employ existing tooling like editors, linters, etc. In this case we use Emacs’s syntax for specifying modes, using a special syntax anywhere in the first line:

    # -*- mode: my-lang -*-

(You can consult the [Emacs manual][] for the details of the syntax.)

The advantage of this approach is that the sequence between `-*-` markers does not have to appear at the beginning; it can be commented out using the appropriate comment syntax. (If the file starts with a shebang (`#!`), the mode can also be specified in the second line, but this is also true of hash langs.)

Lastly, the language of a module can be specified as part of the import syntax. This lets you use files as modules without having to edit them at all, which may be useful for shared files you cannot edit.

    (vernacular:import facts
      :as :cl-yesql/sqlite
      ...)

## Advanced imports

You can use `import-as-package` to import a Vernacular module as a Lisp package:

    (vernacular:import-as-package :facts
      :from "sqlite.sql"
      :binding :all-functions)

This will bind `facts:add-fact` etc.

(If your Lisp supports hierarchical packages, you might find `import-as-subpackage` more useful.)

Vernacular supports local imports using the `with-imports` form:

    (vernacular:with-imports (facts
                              :from "sqlite.sql"
                              :binding :all-functions)
      ...)

This is local in extent, but it supports all the same options as `vernacular:import`.

The `binding` clause of an import form is actually a DSL for import sets, loosely based on [R6RS][r6rs-imports].

A list of names is just a list of imports:

    (x y z) ; x, y, and z as variables

Names can be aliased:

    ((x :as eks) (y :as why) (z :as zed))

You can import the same binding more than once under different aliases:

    ((x :as x1) (x :as x2))

Names can be namespaced:

    ;; x as a variable, y as a function, z as a macro
    (x #'y (macro-function z))

What this does depends on whether the language uses namespaces. If it exports a function named `y`, that is what you get. If it doesn’t use namespaces, then you get the variable `y` in the function namespace.

Namespaces and renaming can be combined:

    ((y :as #'why) (#'z :as zed))

Now the variable named `y` is being imported as a function named `why`, and the function named `z` is being imported as a variable named `zed`.

There are shorthands for importing everything:

    :all ; all the variables, as variables
    :all-functions ; all the functions, as functions
    :all-setters ; all the setf functions
    :all-as-functions ; all the exports, as functions
    :all-as-setters ; all the exports, as setters

These can be combined, with each other and with regular names:

    (:all-functions :all-setters x y z)

You can limit these using `:except`:

    (:except :all x y) ; All variables except x and y.

You can add a prefix to everything:

    ; Import all functions, adding sql- as a prefix.
    (:prefix :all-functions sql-)

Or you can drop a prefix:

    ; Import all functions, removing any get- prefix.
    (:drop-prefix :all-functions get-)

# Writing languages

## Why Vernacular?

## Languages

In Vernacular, a language is just a package. The package exports a reader and an expander. The function bound to the symbol named `read-module` is the *package reader*. The macro bound to the symbol named `module-progn` is the *package expander*.

The important thing: when the package’s reader is called, that same package is also bound as the *current* package. It is then the responsibility of the reader to make sure any symbols it reads in, or inserts into the expansion, are interned in the correct package. (There is a shortcut for this, `vernacular:reintern`.)

(There is one exception to the rule of *language=package*. If another package exists, having the same name, but ending in `-user`, and this other package inherits from the original package, then this *user package* is the package that is made current while reading (and expanding). E.g. a file beginning with `#lang cl` would actually be read in using the `cl-user` package, not the `cl` package itself.)

Note that the reader is responsible for returning a single form, which is the module. That is, the form returned by the package reader should already be wrapped in the appropriate `module-progn`. The exported binding for `module-progn` is *only* looked up when the language is being used as the expander for a meta-language.

(Meta-languages are for language authors who want to reuse an existing syntax.)

## Language packages

Any package can be used as a language, but it can only be used as a hash lang if its name is limited to certain characters (`[a-zA-Z0-9/_+-]`). (Of course this name can also be a (global) nickname.)

(Note that package names are absolute, even on a Lisp that supports [package-local nicknames][].)

It is recommended, although not required, that your language package inherit from `vernacular/cl` rather than from `cl`. The result is the same, except that `vernacular/cl` shadows Common Lisp’s binding and definition forms so they can be locally rebound by language implementations.

The package must export a binding for both `read-module`, a function, and `module-progn`, a macro.

### Meta-languages

### Static exports

If the syntax of your language makes it possible to determine exports statically, you should also define and export `static-exports`. If your language defines `static-exports`, then Vernacular can statically check the validity of import forms.

(This also has implications for phasing. If your language *doesn’t* provide a `static-exports` binding, then the only way Vernacular can expand a request to import everything from a module is by loading that module *at compile time* to get a list of its exports.)

## Simple modules

Most of the time, your language’s package expander will return a `simple-module` form.

    (vernacular:simple-module (#'moo)
      (defun make-moo (o)
        (concat "M" (make-string o :initial-element #\o)))

      (defun moo (&optional (o 2))
        (print (make-moo o))))

This exports a single name, `moo`, bound to a function that says “Moo” with a varying amount of “oo”.

You can define and use macros in a simple module (with `defmacro`), but you can’t export them, and they have to come first. These are limitations you can get around, but you’ll need something more complex (see below).

## Module objects

Vernacular’s module system is based on the generic functions `module-ref`, `module-ref-ns`, and `module-exports`.

### Export names and packages

## Macro exports

Vernacular allows importing and exporting macros. The ability to export macros only becomes useful in the presence of macro hygiene. After experimenting, I have concluded that the right thing, if you want your language to support macros, is to embed a hygiene-compatible language in Lisp, and then compile your language to that.

I’m not being flippant. Embedding a hygiene-compatible language in CL is not just doable; it’s [already been done][HCL]. As a proof of concept, I have adapted Pascal’s Costanza’s hygiene-compatible implementation of [ISLISP][] in Common Lisp (“[Core Lisp][Core Lisp home]”) to work with Vernacular’s module system. This version of Core Lisp lives in [its own repository][Core Lisp].

<!-- NB Don’t remove links, even if they’re not currently being used.
You might want them again later. -->

[Overlord]: https://github.com/ruricolist/overlord
[Lisp1.5]: http://www.softwarepreservation.org/projects/LISP/lisp15_family#Lisp_15_Programmers_Manual_
[phase separation]: http://www.phyast.pitt.edu/~micheles/scheme/scheme21.html
[language tower]: www.phyast.pitt.edu/~micheles/scheme/scheme22.html
[ASDF]: https://common-lisp.net/project/asdf/
[Racket]: https://racket-lang.org/
[Redo]: https://github.com/apenwarr/redo
[implicit phasing]: http://www.cs.indiana.edu/~dyb/pubs/implicit-phasing.pdf
[burgled-batteries]: https://github.com/pinterface/burgled-batteries
[Bosom Serpent]: http://github.com/ruricolist/bosom-serpent
[yesql]: https://github.com/krisajenkins/yesql
[cl-yesql]: http://github.com/ruricolist/cl-yesql
[HTTPS Everywhere]: https://github.com/EFForg/https-everywhere
[cl-https-everywhere]: http://github.com/ruricolist/cl-https-everywhere
[Instaparse]: https://github.com/Engelberg/instaparse
[Pseudoscheme]: https://github.com/sharplispers/pseudoscheme
[ragg]: http://www.hashcollision.org/ragg/
[shlex]: https://github.com/python/cpython/blob/master/Lib/shlex.py
[HCL]: http://www.jucs.org/jucs_16_2/embedding_hygiene_compatible_macros
[Shen]: http://www.shenlanguage.org/
[Serapeum]: https://github.com/ruricolist/serapeum
[at-exp]: https://docs.racket-lang.org/scribble/reader-internals.html
[CL-JavaScript]: http://marijnhaverbeke.nl/cl-javascript/
[Snowball]: http://snowballstem.org
[explicit renaming]: https://doi.org/10.1145/1317265.1317269
[Core Lisp home]: http://www.p-cos.net/core-lisp.html
[r6rs-imports]: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_chap_7
[package-local nicknames]: http://sbcl.org/manual/index.html#Package_002dLocal-Nicknames
[Grosskurth]: https://uwspace.uwaterloo.ca/handle/10012/2673
[apenwarr]: https://github.com/apenwarr/redo
[Ghuloum]: https://dl.acm.org/citation.cfm?id=1626863
[submodules]: https://dl.acm.org/citation.cfm?id=2517211
[YWIW]: https://dl.acm.org/citation.cfm?id=581486
[Racket Manifesto]: http://www.ccs.neu.edu/home/matthias/manifesto/
[ISLISP]: http://islisp.info/
[Core Lisp]: http://github.com/ruricolist/core-lisp
[SLIME]: http://common-lisp.net/project/slime/
[SLY]: https://github.com/joaotavora/sly
[Gasbichler]: https://pdfs.semanticscholar.org/8af5/fbb7988f83baa5a6c3e93e0db4c381abfc3a.pdf
[Bawden]: https://people.csail.mit.edu/alan/mtt/
[Frink]: https://frinklang.org
[LoL]: http://www.letoverlambda.com/
[djb-redo]: https://cr.yp.to/redo.html
[djb]: https://cr.yp.to/djb.html
[Beautiful Racket]: http://beautifulracket.com
[Maxima]: https://sourceforge.net/projects/maxima/
[ACL2]: https://www.cs.utexas.edu/users/moore/acl2/
[hopeless]: https://gist.github.com/samth/3083053
[parser generator]: http://cliki.net/parser%20generator
[Boot]: http://boot-clj.com
[Make]: https://www.gnu.org/software/make/
[Roswell]: https://github.com/roswell/roswell
[cl-launch]: http://cliki.net/cl-launch
[dev]: https://github.com/ruricolist/overlord/tree/dev
[Quicklisp]: https://www.quicklisp.org/beta/
[wiki]: https://github.com/ruricolist/overlord/wiki
[Proctor]: https://github.com/ruricolist/proctor
[Emacs manual]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Choosing-Modes.html#Choosing-Modes
[CL-PPCRE]: https://edicl.github.io/cl-ppcre/

<!-- NB Don’t remove links, even if they’re not currently being used.
You might want them again later. -->
