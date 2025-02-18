let paradigms =
      < imperative
      | declarative
      | functional
      | array
      | objectOriented
      | logic
      | stack
      >

let compilationTarget =
      < machineCode
      | jvmBytecode
      | beamBytecode
      | cli
      | javascript
      | interpreted
      | webassembly
      >

let typingSystem =
      < static
      | dynamic
      | duck
      | strong
      | weak
      | gradual
      | nominal
      | structural
      | inferred
      | dependent
      >

in  [ { name = "Agda"
      , originalAuthors = [ "Ulf Norell", "Catarina Coquand" ]
      , paradigms = [ paradigms.functional ]
      , examples =
            [ ''
              module hello-world where

              open import Agda.Builtin.IO using (IO)
              open import Agda.Builtin.Unit using (⊤)
              open import Agda.Builtin.String using (String)

              postulate putStrLn : String → IO ⊤
              {-# FOREIGN GHC import qualified Data.Text as T #-}
              {-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

              main : IO ⊤
              main = putStrLn "Hello, World!"
              ''
            ]
          : List Text
      , description =
          ''
          A dependently typed functional programming language and proof assistant. Agda is used for writing and verifying mathematical proofs and functional programs.
          ''
      , yearFirstPublished = 1999
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.dependent ]
      , resources =
        [ { title = "Agda Homepage"
          , url = "https://wiki.portal.chalmers.se/agda/"
          }
        , { title = "Agda Documentation", url = "https://agda.readthedocs.io/" }
        , { title = "Agda GitHub Repository"
          , url = "https://github.com/agda/agda"
          }
        ]
      }
    , { name = "ALGOL 58"
      , originalAuthors =
        [ "John Backus"
        , "Friedrich Bauer"
        , "Charles Katz"
        , "Alan Perlis"
        , "Heinz Rutishauser"
        , "Klaus Samelson"
        , "Joseph Wegstein"
        , "Hermann Bottenbruch"
        ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          Originally termed International Algebraic Language (IAL), ALGOL 58 was an attempt to create a universal programming language.  The Communications of the ACM (CACM) used ALGOL notation to publish algorithms.  ALGOL 58 was rapidly succeeded by ALOGOL 60.
          ''
      , yearFirstPublished = 1958
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources = [] : List { title : Text, url : Text }
      }
    , { name = "ALGOL 60"
      , originalAuthors =
        [ "John Backus"
        , "Friedrich Bauer"
        , "Julien Green"
        , "Charles Katz"
        , "John McCarthy"
        , "Peter Naur"
        , "Alan Perlis"
        , "Heinz Rutishauser"
        , "Klaus Samelson"
        , "Adriaan \"Aad\" van Winjgaarden"
        , "Bernard Vauquois"
        , "Joseph Wegstein"
        , "Michael Woodger"
        ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          The ALGOrithmic Language 1960, a further development upon ALGOL 1958.
          ''
      , yearFirstPublished = 1960
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ALGOL 60 Report"
          , url = "https://www.masswerk.at/algol60/report.htm"
          }
        ]
      }
    , { name = "ALGOL 68"
      , originalAuthors =
        [ "Adriaan \"Aad\" van Winjgaarden"
        , "Barry Mailloux"
        , "John E. L. Peck"
        , "Cornelis H. A. Koster"
        ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          The ALGOrithmic Language 1968, a further development upon ALGOL 1960.
          ''
      , yearFirstPublished = 1968
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ALGOL 68 Genie"
          , url = "https://jmvdveer.home.xs4all.nl/algol.html"
          }
        ]
      }
    , { name = "APL"
      , originalAuthors = [ "Kenneth Iverson" ]
      , paradigms = [ paradigms.declarative, paradigms.array ]
      , examples = [] : List Text
      , description =
          ''
          A Programming Language, originally a math notation, is an language specialized for manipulating multidimensional arrays.
          ''
      , yearFirstPublished = 1966
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Dyalog APL", url = "https://www.dyalog.com/" }
        , { title = "TryAPL", url = "https://tryapl.org/" }
        ]
      }
    , { name = "Ada"
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , originalAuthors = [ "Jean Ichbiah et al" ]
      , examples = [] : List Text
      , description =
          ''
          A programming language commission by the United States Department of Defense (DoD) to supercede the variety of languages in use at the time.  Originally designed for embedded systems, Ada emphasizes safety and security through strong typing, explicit concurrency, and protected objects.  Ada is named for Ada Lovelace.
          ''
      , yearFirstPublished = 1980
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Ada Information Clearinghouse"
          , url = "https://www.adaic.org/"
          }
        , { title = "Learn Ada", url = "https://learn.adacore.com/" }
        ]
      }
    , { name = "Assembly"
      , originalAuthors = [ "Kathleen Booth", "Andrew Donald Booth" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          Generally a notation for machine language, assigning readable names (e.g. \"mov\") to machine instructors.  Modern assembly languages often feature programmer conveniences such as labels and macros.
          ''
      , yearFirstPublished = 1947
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.weak ]
      , resources = [] : List { title : Text, url : Text }
      }
    , { name = "B"
      , originalAuthors = [ "Ken Thompson", "Dennis Ritchie" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A simplification of BCPL; predecessor to C.
          ''
      , yearFirstPublished = 1969
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.weak ]
      , resources = [] : List { title : Text, url : Text }
      }
    , { name = "BASIC"
      , originalAuthors = [ "John G. Kemeny", "Thomas E. Kurtz" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          Beginner's All-purpose Symbolic Instruction Code (BASIC) was designed to be easy to learn and use. It became widely popular in the home computer era of the 1980s.
          ''
      , yearFirstPublished = 1964
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources = [] : List { title : Text, url : Text }
      }
    , { name = "BCPL"
      , originalAuthors = [ "Martin Richards" ]
      , description =
          ''
          Basic Combined Programming Language; a language originally designed to implement compilers in.  BCPL was later simplified into B, which was then developed into the still-widely-used C programming language.
          ''
      , paradigms = [ paradigms.imperative ]
      , yearFirstPublished = 1967
      , examples = [] : List Text
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.weak ]
      , resources =
        [ { title = "BCPL Reference Manual"
          , url = "https://www.cl.cam.ac.uk/~mr10/bcplman.pdf"
          }
        ]
      }
    , { name = "C"
      , paradigms = [ paradigms.imperative ]
      , originalAuthors = [ "Dennis Ritchie" ]
      , yearFirstPublished = 1972
      , description =
          ''
          Originally developed in service of developing the UNIX operating system, C is one of the most influential and widely-used programming languages today.
          C was originally standardized by the ANSI in 1989 (C89), but that standard was later superceded by the ISO in 1990 and since.''
      , examples =
            [ ''
              #include <stdio.h>

              void main() {
                printf("Hello, world!\n");
              }
                ''
            ]
          : List Text
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.weak ]
      , resources =
        [ { title = "C Programming Language Standard"
          , url = "https://www.iso.org/standard/82075.html"
          }
        , { title = "Learn C", url = "https://www.learn-c.org/" }
        ]
      }
    , { name = "C#"
      , originalAuthors =
        [ "Anders Hejlsberg", "Scott Wiltamuth", "Peter Golde" ]
      , paradigms =
        [ paradigms.objectOriented
        , paradigms.imperative
        , paradigms.functional
        , paradigms.declarative
        ]
      , examples = [] : List Text
      , description =
          ''
          A modern, object-oriented language developed by Microsoft as part of the .NET platform. While initially similar to Java, C# has evolved to incorporate many functional programming features.
          ''
      , yearFirstPublished = 2000
      , compilationTargets = [ compilationTarget.cli ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "C# Documentation"
          , url = "https://docs.microsoft.com/en-us/dotnet/csharp/"
          }
        , { title = "Learn C#"
          , url = "https://dotnet.microsoft.com/learn/csharp"
          }
        ]
      }
    , { name = "C++"
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , originalAuthors = [ "Bjarne Stroustroup" ]
      , yearFirstPublished = 1985
      , description =
          ''
          An extension of C.  Originally the extension was first-class object orientation (the code `c++` in C means \"add one to c\"), C++ now offers a large number and variety of extensions, including closures, templates, and exceptions.
          ''
      , examples = [] : List Text
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "C++ Standard", url = "https://isocpp.org/" }
        , { title = "CPP Reference", url = "https://en.cppreference.com/" }
        ]
      }
    , { name = "COBOL"
      , originalAuthors =
        [ "Howard Bromberg"
        , "Norman Discount"
        , "Vernon Reeves"
        , "Jean E. Sammet"
        , "William Selden"
        , "Gertrude Tierney"
        , "Grace Hopper"
        ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          COmmon Business-Oriented Language, designed for business use.
          ''
      , yearFirstPublished = 1959
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "COBOL Documentation"
          , url = "https://www.ibm.com/docs/en/cobol-zos"
          }
        ]
      }
    , { name = "Clojure"
      , originalAuthors = [ "Rich Hickey" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A modern dialect of Lisp that runs on the Java Virtual Machine, emphasizing functional programming and immutability.
          ''
      , yearFirstPublished = 2007
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.javascript ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Clojure Homepage", url = "https://clojure.org/" }
        , { title = "Clojure Documentation"
          , url = "https://clojure.org/reference/documentation"
          }
        ]
      }
    , { name = "Common Lisp"
      , originalAuthors =
        [ "Scott Fahlman"
        , "Richard P. Gabriel"
        , "David A. Moon"
        , "Guy L. Steele"
        , "Dan Weinreb"
        , "Kent Pitman"
        ]
      , paradigms =
        [ paradigms.functional, paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A standardized dialect of Lisp that unified several existing Lisp implementations.  It is a multi-paradigm language featuring a macro system, dynamic typing, and support for multiple programming paradigms.
          ''
      , yearFirstPublished = 1984
      , compilationTargets =
        [ compilationTarget.machineCode, compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Common Lisp HyperSpec"
          , url = "http://www.lispworks.com/documentation/HyperSpec/Front/"
          }
        , { title = "Common Lisp Wiki", url = "https://www.cliki.net/" }
        ]
      }
    , { name = "Crystal"
      , originalAuthors =
        [ "Ary Borenszweig", "Juan Wajnerman", "Brian Cardiff" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples =
            [ ''
              puts "Hello, World!"
              ''
            ]
          : List Text
      , description =
          ''
          A programming language with Ruby-like syntax that compiles to native code. It aims to provide the productivity of Ruby with the performance and type safety of a compiled language.
          ''
      , yearFirstPublished = 2014
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Crystal Homepage", url = "https://crystal-lang.org/" }
        , { title = "Crystal Documentation"
          , url = "https://crystal-lang.org/reference/"
          }
        ]
      }
    , { name = "Curry"
      , originalAuthors = [ "Michael Hanus" ]
      , paradigms = [ paradigms.functional, paradigms.logic ]
      , examples = [] : List Text
      , description =
          ''
          A declarative programming language that combines functional programming with logic programming features. Named after Haskell B. Curry, it integrates the most important features of functional languages like Haskell and logic languages like Prolog.
          ''
      , yearFirstPublished = 1999
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Curry Homepage", url = "http://www.curry-lang.org/" }
        , { title = "Curry Documentation"
          , url = "http://www.curry-lang.org/documentation/"
          }
        ]
      }
    , { name = "D"
      , originalAuthors = [ "Walter Bright", "Andrei Alexandrescu" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A systems programming language intended as an improvement over C++.
          ''
      , yearFirstPublished = 2001
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "D Language Homepage", url = "https://dlang.org/" }
        , { title = "D Language Documentation"
          , url = "https://dlang.org/documentation.html"
          }
        ]
      }
    , { name = "Dart"
      , originalAuthors = [ "Lars Bak", "Kasper Lund" ]
      , paradigms = [ paradigms.objectOriented, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A client-optimized programming language developed by Google for building web, mobile, and desktop applications. Originally designed as a replacement for JavaScript, it now focuses on being a general-purpose language with strong tooling support.
          ''
      , yearFirstPublished = 2011
      , compilationTargets = [ compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Dart Homepage", url = "https://dart.dev/" }
        , { title = "Dart Documentation", url = "https://dart.dev/guides" }
        ]
      }
    , { name = "Datalog"
      , originalAuthors = [ "Hervé Gallaire", "Jack Minker" ]
      , paradigms = [ paradigms.logic, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''
          A declarative logic programming language that is a subset of Prolog, but typically uses a bottom-up rather than top-down evaluation style.  It is often used as a query language for deductive databases such as Datomic.
          ''
      , yearFirstPublished = 1977
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static ]
      , resources =
        [ { title = "Datalog Educational System"
          , url = "http://datalog.sourceforge.net/"
          }
        ]
      }
    , { name = "Dhall"
      , originalAuthors = [ "Gabriel Gonzalez" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''
          A programmable configuration language that is not Turing complete. Dhall aims to be a standardized configuration language that is guaranteed to terminate and is more expressive than JSON or YAML.
          ''
      , yearFirstPublished = 2017
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Dhall Homepage", url = "https://dhall-lang.org/" }
        , { title = "Dhall Documentation"
          , url = "https://docs.dhall-lang.org/"
          }
        , { title = "Dhall GitHub"
          , url = "https://github.com/dhall-lang/dhall-lang"
          }
        ]
      }
    , { name = "Eiffel"
      , originalAuthors = [ "Bertrand Meyer" ]
      , paradigms = [ paradigms.declarative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          An object-oriented programming language designed to promote software quality through the use of Design by Contract (DbC). Eiffel emphasizes readability, reusability, and reliability.
          ''
      , yearFirstPublished = 1986
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Eiffel Homepage", url = "https://www.eiffel.org/" }
        , { title = "Eiffel Documentation", url = "https://docs.eiffel.org/" }
        ]
      }
    , { name = "Elixir"
      , originalAuthors = [ "José Valim" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''
          A functional programming language with Ruby-like syntax that runs on the BEAM (the Erlang virtual machine).
          ''
      , yearFirstPublished = 2011
      , compilationTargets = [ compilationTarget.beamBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Elixir Homepage", url = "https://elixir-lang.org/" }
        , { title = "Elixir Documentation", url = "https://hexdocs.pm/elixir/" }
        ]
      }
    , { name = "Elm"
      , originalAuthors = [ "Evan Czaplicki" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A functional programming language that compiles to JavaScript, specifically designed for building web browser-based user interfaces. Elm emphasizes simplicity, ease of use, and no runtime exceptions.
          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Elm Homepage", url = "https://elm-lang.org/" }
        , { title = "Elm Guide", url = "https://guide.elm-lang.org/" }
        , { title = "Elm Package Documentation"
          , url = "https://package.elm-lang.org/"
          }
        ]
      }
    , { name = "Erlang"
      , originalAuthors = [ "Joe Armstrong", "Robert Virding", "Mike Williams" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''
          A functional programming language developed at Ericsson, designed for building scalable distributed systems.
          ''
      , yearFirstPublished = 1986
      , compilationTargets = [ compilationTarget.beamBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Erlang Homepage", url = "https://www.erlang.org/" }
        , { title = "Erlang Documentation"
          , url = "https://www.erlang.org/docs"
          }
        ]
      }
    , { name = "F#"
      , originalAuthors = [ "Don Syme" ]
      , paradigms = [ paradigms.functional, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A functional-first programming language for the .NET ecosystem.
          ''
      , yearFirstPublished = 2005
      , compilationTargets = [ compilationTarget.cli ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "F# Homepage", url = "https://fsharp.org/" }
        , { title = "F# Documentation"
          , url = "https://docs.microsoft.com/en-us/dotnet/fsharp/"
          }
        ]
      }
    , { name = "Factor"
      , originalAuthors = [ "Slava Pestov" ]
      , paradigms =
        [ paradigms.functional, paradigms.declarative, paradigms.stack ]
      , examples = [] : List Text
      , description =
          ''
          A stack-oriented programming language with high-level features like dynamic typing, extensible syntax, macros, and garbage collection. Factor emphasizes interactive development and concatenative programming.
          ''
      , yearFirstPublished = 2003
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Factor Homepage", url = "https://factorcode.org/" }
        , { title = "Factor Documentation"
          , url = "https://docs.factorcode.org/"
          }
        , { title = "Factor GitHub Repository"
          , url = "https://github.com/factor/factor"
          }
        ]
      }
    , { name = "Forth"
      , originalAuthors = [ "Charles H. Moore" ]
      , paradigms = [ paradigms.imperative, paradigms.stack ]
      , examples = [] : List Text
      , description =
          ''
          A stack-based programming language emphasizing simplicity and extensibility.
          ''
      , yearFirstPublished = 1970
      , compilationTargets =
        [ compilationTarget.interpreted, compilationTarget.machineCode ]
      , typing = [ typingSystem.weak ]
      , resources =
        [ { title = "Forth Interest Group", url = "http://www.forth.org/" } ]
      }
    , { name = "Fortran"
      , originalAuthors = [ "John Backus" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          The first high-level programming language, designed for scientific computing.
          ''
      , yearFirstPublished = 1957
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Fortran Homepage", url = "https://fortran-lang.org/" }
        , { title = "Fortran Documentation"
          , url = "https://fortran-lang.org/learn/"
          }
        ]
      }
    , { name = "Gleam"
      , originalAuthors = [ "Louis Pilfold" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A statically typed functional programming language for building scalable concurrent systems that runs on the BEAM.
          ''
      , yearFirstPublished = 2016
      , compilationTargets = [ compilationTarget.beamBytecode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "Gleam Homepage", url = "https://gleam.run/" }
        , { title = "Gleam Documentation"
          , url = "https://gleam.run/documentation/"
          }
        ]
      }
    , { name = "Go"
      , originalAuthors = [ "Robert Griesemer", "Rob Pike", "Ken Thompson" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A statically typed, compiled language designed at Google and strongly influenced by C.  However, Go's design more strongly emphasizes simplicity and safety (e.g. Go features garbage collection instead of manual memory management).
          ''
      , yearFirstPublished = 2009
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Go Homepage", url = "https://golang.org/" }
        , { title = "Go Documentation", url = "https://golang.org/doc/" }
        ]
      }
    , { name = "Haskell"
      , originalAuthors =
        [ "Lennart Augustsson"
        , "Dave Barton"
        , "Brian Boutel"
        , "Warren Burton"
        , "Joseph Fasel"
        , "Kevin Hammond"
        , "Ralf Hinze"
        , "Paul Hudak"
        , "John Hughes"
        , "Thomas Johnsson"
        , "Mark Jones"
        , "Simon Peyton Jones"
        , "John Launchbury"
        , "Erik Meijer"
        , "John Peterson"
        , "Alastair Reid"
        , "Colin Runciman"
        , "Philip Wadler"
        ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples =
            [ ''
              main :: IO ()
              main = putStrLn "Hello, World!"
              ''
            ]
          : List Text
      , description =
          ''
          A purely functional programming language with strong static typing and lazy evaluation.
          ''
      , yearFirstPublished = 1990
      , compilationTargets =
        [ compilationTarget.machineCode
        , compilationTarget.javascript
        , compilationTarget.webassembly
        ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "Haskell Homepage", url = "https://www.haskell.org/" }
        , { title = "Haskell Documentation"
          , url = "https://www.haskell.org/documentation/"
          }
        ]
      }
    , { name = "Idris"
      , originalAuthors = [ "Edwin Brady" ]
      , paradigms = [ paradigms.functional ]
      , examples =
            [ ''
              main : IO ()
              main = putStrLn "Hello, World!"
              ''
            , ''
              data Vect : Nat -> Type -> Type where
                Nil : Vect Z a
                (::) : a -> Vect n a -> Vect (S n) a
              ''
            ]
          : List Text
      , description =
          ''
          A general-purpose functional programming language with dependent types, which allows types to be predicated on values. Idris aims to provide a practical programming language with full dependent types.
          ''
      , yearFirstPublished = 2013
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.dependent ]
      , resources =
        [ { title = "Idris Homepage", url = "https://www.idris-lang.org/" }
        , { title = "Idris Documentation"
          , url = "https://idris2.readthedocs.io/en/latest/"
          }
        , { title = "Idris GitHub Repository"
          , url = "https://github.com/idris-lang/Idris2"
          }
        ]
      }
    , { name = "Io"
      , originalAuthors = [ "Steve Dekorte" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A pure object-oriented programming language inspired by Smalltalk, Self, and Lisp. Everything in Io is a message that is passed to objects. It features a small core with highly dynamic and reflective capabilities.
          ''
      , yearFirstPublished = 2002
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Io Language", url = "https://iolanguage.org/" }
        , { title = "Io Guide"
          , url = "https://iolanguage.org/guide/guide.html"
          }
        ]
      }
    , { name = "Ioke"
      , originalAuthors = [ "Ola Bini" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A prototype-based, dynamic programming language inspired by Io, Smalltalk, Ruby, and Lisp. It runs on the Java Virtual Machine and emphasizes expressiveness and experimentation.
          ''
      , yearFirstPublished = 2008
      , compilationTargets = [ compilationTarget.jvmBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Ioke Language", url = "https://ioke.org/" }
        , { title = "Ioke Documentation"
          , url = "https://ioke.org/documentation.html"
          }
        ]
      }
    , { name = "J"
      , originalAuthors = [ "Kenneth E. Iverson", "Roger Hui" ]
      , paradigms =
        [ paradigms.array, paradigms.declarative, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A successor to APL focusing on array programming.
          ''
      , yearFirstPublished = 1990
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "J Homepage", url = "https://www.jsoftware.com/" }
        , { title = "J Documentation"
          , url = "https://code.jsoftware.com/wiki/Main_Page"
          }
        ]
      }
    , { name = "Java"
      , originalAuthors = [ "James Gosling" ]
      , paradigms = [ paradigms.objectOriented, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A class-based, object-oriented programming language designed to be 'write once, run anywhere' via the Java Virtual Machine.
          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.jvmBytecode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.nominal ]
      , resources =
        [ { title = "Java Homepage", url = "https://www.java.com/" }
        , { title = "Java Documentation"
          , url = "https://docs.oracle.com/en/java/"
          }
        ]
      }
    , { name = "JavaScript"
      , originalAuthors = [ "Brendan Eich" ]
      , paradigms =
        [ paradigms.imperative, paradigms.functional, paradigms.objectOriented ]
      , examples =
            [ ''
              console.log("Hello, World!");
              ''
            ]
          : List Text
      , description =
          ''
          A high-level, multi-paradigm programming language that powers most interactive websites. Originally created for Netscape Navigator, it has since become the de facto language of the web.
          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.javascript ]
      , typing = [ typingSystem.dynamic, typingSystem.weak ]
      , resources =
        [ { title = "MDN JavaScript Guide"
          , url =
              "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide"
          }
        , { title = "ECMAScript Specification"
          , url =
              "https://www.ecma-international.org/publications-and-standards/standards/ecma-262/"
          }
        ]
      }
    , { name = "K"
      , originalAuthors = [ "Arthur Whitney" ]
      , paradigms = [ paradigms.array, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''
          An array programming language used primarily in financial applications.
          ''
      , yearFirstPublished = 1993
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources = [ { title = "Kx Systems", url = "https://kx.com/" } ]
      }
    , { name = "Koka"
      , originalAuthors = [ "Daan Leijen" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A strongly-typed functional programming language with a focus on effect systems and algebraic effects. Koka aims to provide a clear separation between pure and effectful computations.
          ''
      , yearFirstPublished = 2014
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "Koka Homepage", url = "https://koka-lang.github.io/koka/" }
        , { title = "Koka Documentation"
          , url = "https://koka-lang.github.io/koka/doc/book.html"
          }
        , { title = "Koka GitHub Repository"
          , url = "https://github.com/koka-lang/koka"
          }
        ]
      }
    , { name = "Kotlin"
      , originalAuthors = [ "JetBrains Team" ]
      , paradigms =
        [ paradigms.objectOriented
        , paradigms.functional
        , paradigms.imperative
        , paradigms.declarative
        ]
      , examples = [] : List Text
      , description =
          ''
          A modern programming language targeting the JVM, Android, and web browsers (via compilation to JavaScript).
          ''
      , yearFirstPublished = 2011
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Kotlin Homepage", url = "https://kotlinlang.org/" }
        , { title = "Kotlin Documentation"
          , url = "https://kotlinlang.org/docs/"
          }
        ]
      }
    , { name = "Lua"
      , originalAuthors =
        [ "Roberto Ierusalimschy"
        , "Waldemar Celes"
        , "Luiz Henrique de Figueiredo"
        ]
      , paradigms = [ paradigms.imperative, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A lightweight, high-level scripting language designed primarily for embedded use in applications. Known for its efficiency, portability, and ease of integration.
          ''
      , yearFirstPublished = 1993
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Lua Homepage", url = "https://www.lua.org/" }
        , { title = "Lua Documentation", url = "https://www.lua.org/docs.html" }
        ]
      }
    , { name = "MATLAB"
      , originalAuthors = [ "Cleve Moler" ]
      , paradigms = [ paradigms.array, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          Originally \"MATrix LABoratory\", MATLAB is a closed-source numerical computing environment and programming language.
          ''
      , yearFirstPublished = 1984
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "MATLAB Homepage"
          , url = "https://www.mathworks.com/products/matlab.html"
          }
        , { title = "MATLAB Documentation"
          , url = "https://www.mathworks.com/help/matlab/"
          }
        ]
      }
    , { name = "Magik"
      , originalAuthors = [ "Arthur Chance" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A dynamic object-oriented programming language originally developed by Smallworld for developing geographical information systems (GIS). Features multiple inheritance, dynamic typing, and garbage collection.
          ''
      , yearFirstPublished = 1990
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Magik Development Tools (MDT)", url = "https://mdt.net" }
        , { title = "Visual Studio Code Extension"
          , url =
              "https://marketplace.visualstudio.com/items?itemName=siamz.smallworld-magik"
          }
        ]
      }
    , { name = "Mercury"
      , originalAuthors = [ "Zoltan Somogyi", "Ralph Becket", "Peter Ross" ]
      , paradigms = [ paradigms.functional, paradigms.logic ]
      , examples = [] : List Text
      , description =
          ''
          A pure logic programming language that combines the clarity and expressiveness of declarative programming with advanced static analysis and error detection features. Mercury improves upon Prolog with a strong type system, mode system, and determinism analysis.
          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Mercury Homepage", url = "https://mercurylang.org/" }
        , { title = "Mercury Documentation"
          , url = "https://mercurylang.org/documentation/documentation.html"
          }
        ]
      }
    , { name = "Modula"
      , originalAuthors = [ "Niklaus Wirth" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A successor to Pascal emphasizing modularity, wherein groups of related declarations are grouped into modules.
          ''
      , yearFirstPublished = 1975
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources = [] : List { title : Text, url : Text }
      }
    , { name = "Modula-2"
      , originalAuthors = [ "Niklaus Wirth" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A successor to Modula with improved type safety and module system. Modula-2 added support for separate compilation, co-routines for concurrent programming, and a more sophisticated module system. It influenced later languages like Ada and Oberon.
          ''
      , yearFirstPublished = 1978
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ADW Modula-2 Homepage", url = "http://www.modula2.org/" } ]
      }
    , { name = "OCaml"
      , originalAuthors =
        [ "Xavier Leroy"
        , "Jérôme Vouillon"
        , "Damien Doligez"
        , "Didier Rémy"
        , "Ascánder Suárez"
        ]
      , paradigms =
        [ paradigms.functional, paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          Formerly Objective Caml, OCaml is a general-purpose programming language that supports functional, imperative, and object-oriented programming styles. OCaml emphasizes type safety and expressiveness while maintaining high performance through native code compilation.
          ''
      , yearFirstPublished = 1996
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "OCaml Homepage", url = "https://ocaml.org/" }
        , { title = "OCaml Documentation", url = "https://ocaml.org/docs/" }
        , { title = "Real World OCaml (book)"
          , url = "https://dev.realworldocaml.org/"
          }
        ]
      }
    , { name = "Oberon"
      , originalAuthors = [ "Niklaus Wirth" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A successor to Modula-2 that introduced object-oriented features while maintaining simplicity and efficiency. It was designed alongside the Oberon operating system.
          ''
      , yearFirstPublished = 1987
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ETH Oberon"
          , url = "https://www.inf.ethz.ch/personal/wirth/Oberon/"
          }
        ]
      }
    , { name = "Octave"
      , originalAuthors = [ "John W. Eaton" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A high-level language, primarily used for numerical computations.  Octave is a free and open-source alternative to MATLAB.
          ''
      , yearFirstPublished = 1992
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "GNU Octave", url = "https://www.gnu.org/software/octave/" }
        , { title = "Octave Documentation"
          , url = "https://octave.org/doc/interpreter/"
          }
        ]
      }
    , { name = "PHP"
      , originalAuthors = [ "Rasmus Lerdorf" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A server-side scripting language originally designed for web development. PHP originally stood for \"Personal Home Page\" but was later renamed to \"PHP: Hypertext Preprocessor\".
          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.weak ]
      , resources =
        [ { title = "PHP Homepage", url = "https://www.php.net/" }
        , { title = "PHP Documentation", url = "https://www.php.net/docs.php" }
        ]
      }
    , { name = "PL/I"
      , originalAuthors = [ "IBM" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          Programming Language One, designed by IBM and its user group, SHARE, to succeed FORTRAN and COBOL.
          ''
      , yearFirstPublished = 1964
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static ]
      , resources = [] : List { title : Text, url : Text }
      }
    , { name = "Pascal"
      , originalAuthors = [ "Niklaus Wirth" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A language designed to encourage good programming practices.  Wirth's efforts were originally part of the effort to design the next version of ALGOL (the ALGOL X effort), but Wirth's proposals were not accepted, so he created Pascal instead.  Pascal was very popular in the 1970s and 1980s, but has since been largely supplanted by C and C++.
          ''
      , yearFirstPublished = 1970
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Wikipedia: Pascal"
          , url = "https://en.wikipedia.org/wiki/Pascal_(programming_language)"
          }
        , { title = "Free Pascal (a Pascal compiler)"
          , url = "https://www.freepascal.org/"
          }
        ]
      }
    , { name = "Perl"
      , originalAuthors = [ "Larry Wall" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A general-purpose language originally designed for text processing.  Perl is known for its flexibility and its ability to easily create domain-specific languages.
          ''
      , yearFirstPublished = 1987
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Perl Homepage", url = "https://www.perl.org/" }
        , { title = "Perl Documentation", url = "https://perldoc.perl.org/" }
        ]
      }
    , { name = "Pony"
      , originalAuthors = [ "Sebastian Blessing", "Sylvan Clebsch" ]
      , paradigms = [ paradigms.objectOriented, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          An object-oriented programming language designed for writing safe, high-performance actor-based programs. Pony emphasizes capabilities-secure type system and data-race freedom through its reference capabilities system.
          ''
      , yearFirstPublished = 2015
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.nominal ]
      , resources =
        [ { title = "Pony Homepage", url = "https://www.ponylang.io/" }
        , { title = "Pony Tutorial", url = "https://tutorial.ponylang.io/" }
        , { title = "Pony Documentation"
          , url = "https://www.ponylang.io/learn/"
          }
        ]
      }
    , { name = "Prolog"
      , originalAuthors = [ "Alain Colmerauer", "Philippe Roussel" ]
      , paradigms = [ paradigms.logic, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''
          A logic programming language based on first-order predicate calculus. Programs consist of facts and rules, and computation proceeds by making logical queries against this knowledge base.
          ''
      , yearFirstPublished = 1972
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "SWI-Prolog (a Prolog implementation)"
          , url = "https://swi-prolog.org"
          }
        , { title = "Prolog Tutorial"
          , url = "https://www.swi-prolog.org/pldoc/man?section=quickstart"
          }
        , { title = "GNU Prolog (a Prolog implementation)"
          , url = "http://gprolog.org/"
          }
        ]
      }
    , { name = "PureScript"
      , originalAuthors = [ "Phil Freeman" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A strongly-typed functional programming language that compiles to JavaScript. PureScript is similar to Haskell but designed specifically for the web platform, featuring row polymorphism, type classes, and strict evaluation.
          ''
      , yearFirstPublished = 2013
      , compilationTargets = [ compilationTarget.javascript ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "PureScript Homepage", url = "https://www.purescript.org/" }
        , { title = "PureScript Documentation"
          , url = "https://github.com/purescript/documentation"
          }
        , { title = "Try PureScript", url = "https://try.purescript.org/" }
        ]
      }
    , { name = "Pyret"
      , originalAuthors =
        [ "Joe Gibbs Politz"
        , "Benjamin Lerner"
        , "Daniel Patterson"
        , "Dorai Sitaram"
        ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A programming language designed for teaching computer science, featuring built-in testing, tables, and image manipulation. Pyret emphasizes clear error messages and integrates features from both functional and object-oriented programming.
          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Pyret Homepage", url = "https://www.pyret.org/" }
        , { title = "Pyret Documentation", url = "https://www.pyret.org/docs/" }
        , { title = "Online Editor", url = "https://code.pyret.org/" }
        ]
      }
    , { name = "Python"
      , originalAuthors = [ "Guido van Rossum" ]
      , paradigms =
        [ paradigms.imperative, paradigms.objectOriented, paradigms.functional ]
      , examples = [ "print(\"Hello, World!\")" ]
      , description =
          ''
          A high-level, general-purpose programming language emphasizing code readability with its notable use of significant whitespace. Python supports multiple programming paradigms and features a comprehensive (frequently referred to as \"batteries-included\") standard library.
          ''
      , yearFirstPublished = 1991
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Python Homepage", url = "https://www.python.org/" }
        , { title = "Python Documentation", url = "https://docs.python.org/" }
        ]
      }
    , { name = "QBasic"
      , originalAuthors = [ "Microsoft" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          An IDE and interpreter for a version of BASIC developed by Microsoft. It was included with MS-DOS and was many programmers' first introduction to programming.
          ''
      , yearFirstPublished = 1991
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "QB64 (a modern implementation of QBASIC)"
          , url = "https://qb64.com/"
          }
        ]
      }
    , { name = "R"
      , originalAuthors = [ "Ross Ihaka", "Robert Gentleman" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A language for statistical computing and data visualization.
          ''
      , yearFirstPublished = 1993
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "R Homepage", url = "https://www.r-project.org/" }
        , { title = "R Documentation", url = "https://www.rdocumentation.org/" }
        ]
      }
    , { name = "Racket"
      , originalAuthors =
        [ "Mathias Felleisen"
        , "Matthew Flatt"
        , "Robert Bruce Findler"
        , "Shriram Krishnamurthi"
        , "PLT Inc."
        ]
      , paradigms = [ paradigms.functional, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A general-purpose, multi-paradigm programming language in the Lisp/Scheme family. Racket is designed to be a platform for programming language design, implementation, and learning.
          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Racket Homepage", url = "https://racket-lang.org" }
        , { title = "Racket Documentation"
          , url = "https://docs.racket-lang.org"
          }
        ]
      }
    , { name = "Raku"
      , originalAuthors = [ "Larry Wall" ]
      , paradigms = [ paradigms.objectOriented, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A multi-paradigm language formerly known as Perl 6, designed to be more consistent and modern than Perl 5.
          ''
      , yearFirstPublished = 2015
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.dynamic, typingSystem.gradual, typingSystem.strong ]
      , resources =
        [ { title = "Raku Homepage", url = "https://raku.org" }
        , { title = "Raku Documentation", url = "https://docs.raku.org/" }
        ]
      }
    , { name = "Rebol"
      , originalAuthors = [ "Carl Sassenrath" ]
      , paradigms = [ paradigms.functional, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A lightweight messaging language designed for distributed computing and network communications, with a focus on human-readable syntax.  Sassenrath claims its greatest strength is its ability to easily create domain-specific languages.
          ''
      , yearFirstPublished = 1997
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Rebol Homepage", url = "https://www.rebol.com" }
        , { title = "Rebol Documentation"
          , url = "https://www.rebol.com/docs.html"
          }
        ]
      }
    , { name = "Red"
      , originalAuthors = [ "Nenad Rakočević" ]
      , paradigms = [ paradigms.functional, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A next-generation programming language strongly inspired by Rebol but with a focus on cross-compilation, concurrency and high performance.
          ''
      , yearFirstPublished = 2011
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Red Homepage", url = "https://www.red-lang.org" }
        , { title = "Red Documentation", url = "https://docs.red-lang.org" }
        , { title = "GitHub Repository", url = "https://github.com/red/red" }
        ]
      }
    , { name = "Ruby"
      , originalAuthors = [ "Yukihiro \"Matz\" Matsumoto" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [ "puts \"Hello World!\"" ]
      , description =
          ''
          A dynamic, object-oriented language emphasizing simplicity and productivity.
          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Ruby Homepage", url = "https://ruby-lang.org" }
        , { title = "Ruby Documentation"
          , url = "https://www.ruby-lang.org/en/documentation/"
          }
        , { title = "Try Ruby (browser REPL)"
          , url = "https://try.ruby-lang.org/"
          }
        ]
      }
    , { name = "Rust"
      , originalAuthors = [ "Graydon Hoare" ]
      , paradigms = [ paradigms.imperative, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A systems programming language focused on safety, concurrency, and performance, originally developed at Mozilla.  Rust is notable for its compile-time memory safety guarantees through a unique ownership system.
          ''
      , yearFirstPublished = 2012
      , compilationTargets =
        [ compilationTarget.machineCode, compilationTarget.webassembly ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Rust Homepage", url = "https://www.rust-lang.org/" }
        , { title = "Rust Documentation", url = "https://doc.rust-lang.org/" }
        , { title = "The Rust Programming Language (book)"
          , url = "https://doc.rust-lang.org/book/"
          }
        ]
      }
    , { name = "SNOBOL"
      , originalAuthors = [ "Ralph Griswold", "Ivan Polonsky", "David Farber" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          StriNg Oriented symBOlic Language, designed for text processing.  SNOBOL had support for \"patterns\", which were more powerful than regular expressions.  SNOBOL was used for text processing in the 1960s and 1970s, but was largely supplanted by Perl and AWK.
          ''
      , yearFirstPublished = 1962
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Wikipedia: SNOBOL"
          , url = "https://en.wikipedia.org/wiki/SNOBOL"
          }
        ]
      }
    , { name = "SQL"
      , originalAuthors = [ "Donald D. Chamberlin", "Raymond F. Boyce" ]
      , paradigms = [ paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''
          Structured Query Language, a domain-specific language designed for managing and querying relational databases. It has become the standard language for relational database management systems, used in PostgreSQL, MySQL, Oracle, and other database systems.
          ''
      , yearFirstPublished = 1974
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "SQL Standard"
          , url = "https://www.iso.org/standard/76583.html"
          }
        ]
      }
    , { name = "Scala"
      , paradigms = [ paradigms.functional, paradigms.objectOriented ]
      , originalAuthors = [ "Martin Odersky" ]
      , examples = [] : List Text
      , description =
          ''
          A language combining object-oriented and functional programming running on the JVM.
          ''
      , yearFirstPublished = 2004
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Scala Homepage", url = "https://www.scala-lang.org/" }
        , { title = "Scala Documentation"
          , url = "https://docs.scala-lang.org/"
          }
        ]
      }
    , { name = "Scheme"
      , originalAuthors = [ "Guy L. Steele", "Gerald Jay Sussman" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A dialect of Lisp emphasizing simplicity and minimalism, widely used in computer science education and research.
          ''
      , yearFirstPublished = 1975
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Wikipedia: Scheme (programming language)"
          , url = "https://en.wikipedia.org/wiki/Scheme_(programming_language)"
          }
        ]
      }
    , { name = "Self"
      , originalAuthors = [ "David Ungar", "Randall Smith" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A prototype-based object-oriented programming language that pioneered many concepts used in modern languages like JavaScript.
          ''
      , yearFirstPublished = 1987
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Wikipedia: Self (programming language)"
          , url = "https://en.wikipedia.org/wiki/Self_(programming_language)"
          }
        ]
      }
    , { name = "Smalltalk"
      , originalAuthors =
        [ "Dan Ingalls"
        , "Alan Kay"
        , "Adele Goldberg"
        , "Ted Kaehler"
        , "Diana Merry"
        , "Scott Wallace"
        ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          One of the first pure object-oriented programming languages.
          ''
      , yearFirstPublished = 1972
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Squeak Homepage (Smalltalk Implementation)"
          , url = "https://squeak.org/"
          }
        , { title = "Pharo Homepage (Smalltalk Implementation)"
          , url = "https://pharo.org/"
          }
        ]
      }
    , { name = "Simula"
      , originalAuthors = [ "Ole-Johan Dahl", "Kristen Nygaard" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          The first object-oriented programming language.
          ''
      , yearFirstPublished = 1965
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Wikipedia: Simula"
          , url = "https://en.wikipedia.org/wiki/Simula"
          }
        ]
      }
    , { name = "Standard ML (SML)"
      , originalAuthors = [ "Robin Milner", "Mads Tofte", "Robert Harper" ]
      , paradigms = [ paradigms.imperative, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          An attempt to \"standardize\" ML implementations, Standard ML (SML) is a general-purpose functional programming language that features static typing, type inference, pattern matching, and a sophisticated module system. SML influenced many modern functional languages including OCaml, F#, and Scala.
          ''
      , yearFirstPublished = 1983
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Standard ML Family GitHub"
          , url = "https://smlfamily.github.io/"
          }
        , { title = "SML/NJ (a compiler for SML)"
          , url = "https://www.smlnj.org/"
          }
        , { title = "MLton (a compiler for SML)", url = "http://mlton.org/" }
        ]
      }
    , { name = "Swift"
      , originalAuthors =
        [ "Chris Lattner"
        , "Doug Gregor"
        , "John McCall"
        , "Ted Kremenek"
        , "Joe Groff"
        ]
      , paradigms = [ paradigms.objectOriented, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A modern programming language developed by Apple as a replacement for Objective-C.  Swift is primarily used to create apps for iOS, macOS, and other Apple operating systems.
          ''
      , yearFirstPublished = 2014
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Swift Homepage", url = "https://www.swift.org/" }
        , { title = "Swift Documentation", url = "https://docs.swift.org/" }
        , { title = "The Swift Programming Language (book)"
          , url =
              "https://docs.swift.org/swift-book/documentation/the-swift-programming-language/"
          }
        , { title = "Apple Documentation"
          , url = "https://developer.apple.com/swift/"
          }
        ]
      }
    , { name = "TypeScript"
      , originalAuthors = [ "Anders Hejlsberg" ]
      , paradigms =
        [ paradigms.objectOriented, paradigms.functional, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A strict syntactical superset of JavaScript that adds optional static typing. Developed by Microsoft to enable JavaScript development at scale.
          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.javascript ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.structural ]
      , resources =
        [ { title = "TypeScript Homepage"
          , url = "https://www.typescriptlang.org/"
          }
        , { title = "TypeScript Documentation"
          , url = "https://www.typescriptlang.org/docs/"
          }
        , { title = "The TypeScript Handbook"
          , url = "https://www.typescriptlang.org/docs/handbook/intro.html"
          }
        ]
      }
    , { name = "Unison"
      , originalAuthors = [ "Paul Chiusano", "Rúnar Bjarnason" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A modern functional programming language that emphasizes immutability, distributed computing, and a unique content-addressed codebase. Unison aims to simplify code collaboration and versioning.
          ''
      , yearFirstPublished = 2019
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Unison Homepage", url = "https://www.unison-lang.org/" }
        , { title = "Unison Documentation"
          , url = "https://www.unison-lang.org/docs/"
          }
        , { title = "Unison Cloud", url = "https://www.unison.cloud/" }
        , { title = "Unison GitHub Repository"
          , url = "https://github.com/unisonweb/unison"
          }
        ]
      }
    , { name = "V"
      , originalAuthors = [ "Alexander Medvednikov" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''
          A systems programming language, very similar to Go.  V emphasizes safety, performance, and simplicity with fast compilation, memory safety without garbage collection, and C-style syntax.
          ''
      , yearFirstPublished = 2019
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "V Programming Language", url = "https://vlang.io/" }
        , { title = "V Documentation", url = "https://docs.vlang.io/" }
        , { title = "V GitHub Repository", url = "https://github.com/vlang/v" }
        , { title = "V Playground (online editor)"
          , url = "https://play.vlang.io/"
          }
        ]
      }
    , { name = "Visual Basic"
      , originalAuthors = [ "Microsoft" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''
          A version of BASIC developed by Microsoft for its COM programming model and Windows GUI development. Later evolved into Visual Basic .NET.
          ''
      , yearFirstPublished = 1991
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static ]
      , resources =
        [ { title = "Visual Basic Documentation"
          , url = "https://learn.microsoft.com/en-us/dotnet/visual-basic/"
          }
        ]
      }
    , { name = "Zig"
      , originalAuthors = [ "Andrew Kelley" ]
      , paradigms = [ paradigms.imperative, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''
          A general-purpose programming language with an emphasis on being explicit. To this end, Zig makes features that are usually implicit, such as memory allocations, explicit. Zig supports incremental adoption into C/C++ codebases.
          ''
      , yearFirstPublished = 2016
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "Zig Homepage", url = "https://ziglang.org" }
        , { title = "Compiler repository"
          , url = "https://github.com/ziglang/zig"
          }
        ]
      }
    ]
