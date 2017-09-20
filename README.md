# Projector

Projector is a typed, total templating language that produces
well-formed HTML.

Concretely, this means:

- Users define data types in a Haskell-like syntax and pass them to Projector
- Users write their markup in a HTML-like syntax, augmented with an
  expression language
- That surface syntax desugars into a typed lambda calculus, which is
  type checked according to Damas-Milner
- The datatypes and typechecked terms are simplified
- A collection of platform-specific backends transform the simplified
  terms into (somewhat) idiomatic native code.

There are a few perks to this approach:

- Templates have to make some semblance of sense
- Variables must be defined and well-typed
- Your frontend views can be seamlessly shared between client and
  server, even if you're using a complicated vdom-based library
- Templates are pure functions that cannot throw errors at runtime
- Infinite loops while rendering are unlikely (disclaimer: this is syntactic)
- Templates can be normalised and simplified at compile time, reducing
  code size and providing runtime optimisation opportunities
- Static analysis of the constructed markup becomes possible
- Very few dependencies are needed at runtime

There are a few things that have not been fleshed out, though
they are fairly straightforward:

- There's no event system right now, though it's coming!
- User data types must be very simple (no polymorphism)
- Type errors are sometimes inscrutable (though usually pretty good)

There are also a few downsides to this approach that are worth your
consideration:

- You're locked into our wacky Haskell-esque syntax (it's serviceable,
  but could use a bit of design iteration)
- Every template requires a type signature (just one, at the top -
  everything else is inferred)
- The interactive stateful component of your application is beyond
  Projector's remit, and is best pursued in your preferred Javascript
  ecosystem.  Consider Projector a language to define views.  It could
  be used with frameworks like React, Redux, Elm, or Pux, but it does
  not replace them.
- The generated code can be a little large, though there's a lot of
  low-hanging fruit to improve this.
    - Currently we rely on beta/eta reduction and a collection of
      rewrite rules to simplify terms. More rules will make a big
      difference.
	- Currently we generate native terms on each platform for each
      template, and have them actually call one another. This
      minimises the recompilation burden during interactive
      development. We'd get smaller, simpler code by instead providing
      entry points, then inlining all the function calls, then
      simplifying.

Projector achieves its goals via a series of well-known implementation
techniques from the field of programming languages. No single element
of Projector is novel.

## Syntax


## Data Types

- Ground / primitives
- Variants
- Records

## Backends

At present there are only two backends:

- Haskell Hydrant
- Purescript Pux 6

Similar platforms like Elm, React, virtual-dom, blaze or lucid would
be pretty easy to support, building off the existing backends. C or
Rust would be a moderate amount of quite interesting work.

## Totality (or lack thereof)

Contrary to the project description, Projector has no totality
checker, and the underlying calculus does admit divergent terms!

However, both syntactic restrictions and practical considerations
mandate that all templates be total. These were quite effective for
the prototype, so nobody bothered to run the last mile and implement a
checker / refine the underlying calculus. It would still be a good
exercise to do so!

In particular, the following features provide a little bit of
assurance that your frontend is total:
- If a template diverges, the simplifier will also diverge, breaking
  the build
- There are no let bindings, so it's hard (impossible?) to diverge
  inside a single file
- The call graph must form a DAG, so it's hard to diverge by
  accidentally-cyclic definitions
- Data types cannot contain Projector functions

However, there is at least one way to create a divergent template. For
now, just try not to hold it wrong.


## Interactive development



## Build Process

Here we outline the build process from strings to code, plus starting
points if you want to hack on Projector:

- Parse datatypes and templates from files/strings
    - `Projector.Html.Syntax` et al, `Projector.Html.Core.Machinator`
- Desugar templates into lambda terms
    - `Projector.Html.Core.Elaborator`
- Typecheck lambda terms
    - Type checker lives in `Projector.Core.Check`, built-in types
      live in `Projector.Html.Core.*`
- Simplify lambda terms (beta, eta, type-preserving rewrites)
    - `Projector.Core.Eval`, `Projector.Core.Rewrite`,
      `Projector.Html.Interpreter`, global rewrite rules live in
      `Projector.Html.Backend.Rewrite`
- Apply a user-supplied naming scheme
    - `ModuleNamer` in `Projector.Html`
- Group terms and types into modules according to a user-supplied heuristic
    - `ModuleNamer` in `Projector.Html`
- Apply backend-specific rewrite rules (non-type-preserving)
- Generate backend code, usually via recursive descent

## Conceptual reviewers

- Tim H
- Charles
- Jacob
