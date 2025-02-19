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

in  [ { name = "8th"
      , originalAuthors = [ "Ron Nicholson" ]
      , paradigms = [ paradigms.stack, paradigms.imperative ]
      , examples =
        [ ''
          "Hello, world!\n" . bye
          ''
        ]
      , description =
          ''

          A cross-platform programming language focused on mobile and desktop
          development with a Forth-like syntax. 8th is a commercial product, but
          offers a free version.

          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.strong ]
      , resources =
        [ { title = "8th Homepage", url = "https://8th-dev.com" }
        , { title = "8th Documentation"
          , url = "https://8th-dev.com/manual.html"
          }
        ]
      }
    , { name = "ABAP"
      , originalAuthors = [ "SAP SE" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          Advanced Business Application Programming, a high-level programming
          language created by SAP for developing business applications in the
          SAP environment.

          ''
      , yearFirstPublished = 1983
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ABAP Documentation"
          , url =
              "https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm"
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

          Originally termed International Algebraic Language (IAL), ALGOL 58 was
          an attempt to create a universal programming language.  Consequently,
          the Communications of the ACM (CACM) used ALGOL notation to publish
          algorithms.  ALGOL 58 was rapidly succeeded by ALOGOL 60.

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
      , examples =
        [ ''
          sum←+/
          sum 10 20 30
          ''
        , ''
          Avg←{(+⌿⍵)÷≢⍵}
          Avg 1 6 3 4
          ''
        ]
      , description =
          ''

          A Programming Language, originally a math notation, is an language
          specialized for manipulating multidimensional arrays. APL uses a
          non-ASCII character set, and historically was programmed using special
          keyboards that provided APL symbols.

          ''
      , yearFirstPublished = 1966
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Dyalog APL", url = "https://www.dyalog.com" }
        , { title = "TryAPL", url = "https://tryapl.org" }
        ]
      }
    , { name = "AWK"
      , originalAuthors =
        [ "Alfred Aho", "Peter Weinberger", "Brian Kernighan" ]
      , paradigms = [ paradigms.declarative ]
      , examples =
        [ ''
          BEGIN { print "Hello, world!" }
          ''
        ]
      , description =
          ''

          A programming language designed for text processing. Named after its
          authors, AWK is based on the model of matching a pattern in input data
          to a corresponding action.

          ''
      , yearFirstPublished = 1977
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "GAWK (GNU AWK) Documentation"
          , url = "https://www.gnu.org/software/gawk/manual/gawk.html"
          }
        ]
      }
    , { name = "Ada"
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , originalAuthors = [ "Jean Ichbiah et al" ]
      , examples = [] : List Text
      , description =
          ''

          A programming language commission by the United States Department of
          Defense (DoD) to supercede the variety of languages in use at the
          time. Originally designed for embedded systems, Ada emphasizes safety
          and security through strong typing, explicit concurrency, and
          protected objects.  Ada is named for Ada Lovelace.

          ''
      , yearFirstPublished = 1980
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Ada Information Clearinghouse"
          , url = "https://www.adaic.org"
          }
        , { title = "Learn Ada", url = "https://learn.adacore.com" }
        ]
      }
    , { name = "Agda"
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
        , ''
          module hello-world-dep where

          open import Data.Nat using (ℕ; zero; suc)

          data Vec (A : Set) : ℕ → Set where
            []  : Vec A zero
            _∷_ : ∀ {n} (x : A) (xs : Vec A n) → Vec A (suc n)

          infixr 5 _∷_
          ''
        ]
      , description =
          ''

          A dependently typed functional programming language and proof
          assistant. Agda is used for writing and verifying mathematical proofs
          and functional programs.

          ''
      , yearFirstPublished = 1999
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.dependent ]
      , resources =
        [ { title = "Agda Homepage"
          , url = "https://wiki.portal.chalmers.se/agda"
          }
        , { title = "Agda Documentation", url = "https://agda.readthedocs.io" }
        , { title = "Agda GitHub Repository"
          , url = "https://github.com/agda/agda"
          }
        , { title = "Programming Language Foundations in Agda"
          , url = "https://plfa.github.io"
          }
        ]
      }
    , { name = "Arturo"
      , originalAuthors = [ "Yanis Zafirópulos" ]
      , paradigms = [ paradigms.functional ]
      , examples =
        [ ''
          factorial: function [n][
              (n > 1)? -> n * factorial n-1
                       -> 1
          ]
          ''
        , ''
          0..10 | map => factorial
                | select.first => [& > 123]
                | print
          ''
        ]
      , description =
          ''

          A programming language focused on simplicity and expressiveness, with
          built-in support for desktop, web, and shell scripting applications.

          ''
      , yearFirstPublished = 2019
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Arturo Homepage", url = "https://arturo-lang.io" }
        , { title = "Arturo Documentation"
          , url = "https://arturo-lang.io/documentation"
          }
        , { title = "Arture Playground"
          , url = "https://arturo-lang.io/playground"
          }
        ]
      }
    , { name = "Assembly"
      , originalAuthors = [ "Kathleen Booth", "Andrew Donald Booth" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          Generally a notation for machine language, assigning readable names
          (e.g. "mov") to machine instructors. Modern assembly languages often
          feature programmer conveniences such as labels and macros.

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

          A simplification of BCPL; predecessor to C. Like BCPL, B featured only
          one data type, the machine word (length of a register). The lack of
          support for ASCII characters was one motivating force behind the
          development of C.

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

          Beginner's All-purpose Symbolic Instruction Code (BASIC) was designed
          to be easy to learn and use. It became widely popular in the home
          computer era of the 1980s.

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

          Basic Combined Programming Language; a language originally designed to
          implement compilers in. BCPL featured only one data type, which was
          the machine word (length of a register). BCPL was later simplified
          into B, which was then developed into the still-widely-used C
          programming language.

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
    , { name = "Ballerina"
      , originalAuthors = [ "WSO2" ]
      , paradigms = [ paradigms.imperative ]
      , examples =
        [ ''
          import ballerina/io;

          public function main() {
              io:println("Hello World!");
          }
          ''
        ]
      , description =
          ''

          An open-source, general-purpose programming language for cloud-native
          development. Ballerina is designed to simplify writing distributed
          programs.

          ''
      , yearFirstPublished = 2017
      , compilationTargets = [ compilationTarget.jvmBytecode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Ballerina Homepage", url = "https://ballerina.io" }
        , { title = "Ballerina Documentation"
          , url = "https://ballerina.io/learn"
          }
        ]
      }
    , { name = "Bash"
      , originalAuthors = [ "Brian Fox" ]
      , paradigms = [ paradigms.imperative ]
      , examples =
        [ ''
          echo "Hello, World!"
          ''
        , ''
          for i in {1..5}; do
            echo "Welcome $i times"
          done
          ''
        ]
      , description =
          ''

          The Bourne Again SHell (Bash) is a Unix shell and command language
          written as a free software replacement for the Bourne shell. It is widely
          available on various operating systems and is the default shell on most
          Linux distributions.

          ''
      , yearFirstPublished = 1989
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Bash Homepage", url = "https://www.gnu.org/software/bash" }
        , { title = "Bash Documentation"
          , url = "https://www.gnu.org/software/bash/manual/bash.html"
          }
        ]
      }
    , { name = "C"
      , paradigms = [ paradigms.imperative ]
      , originalAuthors = [ "Dennis Ritchie" ]
      , yearFirstPublished = 1972
      , description =
          ''

          Originally developed in service of developing the UNIX operating
          system, C is one of the most influential and widely-used programming
          languages today. C was originally standardized by the ANSI in 1989
          (C89), and that standard was later superceded by the ISO in 1990 and
          since.

          ''
      , examples =
        [ ''
          #include <stdio.h>

          void main() {
            printf("Hello, world!\n");
          }
            ''
        ]
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.weak ]
      , resources =
        [ { title = "C Programming Language Standard"
          , url = "https://www.iso.org/standard/82075.html"
          }
        , { title = "Learn C", url = "https://www.learn-c.org" }
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

          A modern, object-oriented language developed by Microsoft as part of
          the .NET platform. While initially similar to Java, C# has evolved to
          incorporate a large variety of programming language features.

          ''
      , yearFirstPublished = 2000
      , compilationTargets = [ compilationTarget.cli ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "C# Documentation"
          , url = "https://docs.microsoft.com/en-us/dotnet/csharp"
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

          An popular and widely-used extension of C. While the first extension
          was first-class object-orientation (the code `c++` in C means "add
          one to c"), C++ now offers a large number and variety of extensions,
          including closures, templates, and exceptions.

          ''
      , examples = [] : List Text
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "C++ Standard", url = "https://isocpp.org" }
        , { title = "CPP Reference", url = "https://en.cppreference.com" }
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

          COmmon Business-Oriented Language, an early programming language
          designed for business use. While rare, some COBOL code is still in use
          today.

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
    , { name = "Cairo"
      , originalAuthors = [ "StarkWare" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          Cairo is a Rust-inspired programming language. It is used as the
          smart contract language for Starknet.

          ''
      , yearFirstPublished = 2020
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Cairo Homepage", url = "https://www.cairo-lang.org" }
        , { title = "The Cairo Book", url = "https://book.cairo-lang.org" }
        , { title = "Cairo Playground (online editor)"
          , url = "https://www.cairo-lang.org/cairovm"
          }
        , { title = "Cairo GitHub Repository"
          , url = "https://github.com/starkware-libs/cairo-lang"
          }
        ]
      }
    , { name = "Ceylon"
      , originalAuthors = [ "Gavin King" ]
      , paradigms = [ paradigms.objectOriented, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          Eclipe Ceylon is a defunct programming language designed for writing
          large programs in teams originally developed by Red Hat. Ceylon
          emphasized readability, modularity, and type safety.

          ''
      , yearFirstPublished = 2011
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Ceylon Homepage", url = "https://ceylon-lang.org" } ]
      }
    , { name = "Chez Scheme"
      , originalAuthors = [ "R. Kent Dybvig" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A high-performance superset of R6RS Scheme known for its speed and reliability. Chez Scheme was originally developed at Cadence Research Systems and was open sourced after Cadence was acquired by Cisco Systems. Chez Scheme ships with a second implementation, named Petite Chez Scheme, which uses a threaded interpreter as opposed to Chez Scheme's incremental native compilation.

          ''
      , yearFirstPublished = 1985
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Chez Scheme", url = "https://cisco.github.io/ChezScheme" }
        , { title = "User's Guide"
          , url = "https://cisco.github.io/ChezScheme/csug9.5/csug.html"
          }
        ]
      }
    , { name = "Chicken Scheme"
      , originalAuthors = [ "Felix Winkelmann" ]
      , paradigms = [ paradigms.functional ]
      , examples =
        [ ''
          (print "Hello, World!")
          ''
        ]
      , description =
          ''

          A Scheme that supports R5RS and R7RS and compiles to C. Known for its extensive library ecosystem and ease of deployment. CHICKEN Scheme aims to be free, simple, portable, extensible, well documented, and actively supported.

          ''
      , yearFirstPublished = 2000
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "CHICKEN Scheme", url = "https://www.call-cc.org" }
        , { title = "Documentation", url = "https://wiki.call-cc.org" }
        ]
      }
    , { name = "Clojure"
      , originalAuthors = [ "Rich Hickey" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A modern dialect of Lisp that runs on the Java Virtual Machine,
          emphasizing functional programming and immutability.

          ''
      , yearFirstPublished = 2007
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.javascript ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Clojure Homepage", url = "https://clojure.org" }
        , { title = "Clojure Documentation"
          , url = "https://clojure.org/reference/documentation"
          }
        ]
      }
    , { name = "CoffeeScript"
      , originalAuthors = [ "Jeremy Ashkenas" ]
      , paradigms = [ paradigms.functional, paradigms.imperative ]
      , examples =
        [ ''
          console.log 'Hello, World!'
          ''
        ]
      , description =
          ''

          A language that compiles to JavaScript, aiming to make code more
          readable and to provide syntactic sugar for common JavaScript idioms.
          Its syntax is inspired by Ruby, Python, and Haskell.

          ''
      , yearFirstPublished = 2009
      , compilationTargets = [ compilationTarget.javascript ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "CoffeeScript Homepage", url = "https://coffeescript.org" }
        , { title = "CoffeeScript Documentation"
          , url = "https://coffeescript.org/#documentation"
          }
        ]
      }
    , { name = "ColdFusion Markup Langauge (CFML)"
      , originalAuthors = [ "J. J. Allaire", "Jeremy Allaire" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          ColdFusion is a web application and mobile development platform
          created by Allaire and now owned by Adobe. ColdFusion Markup Language
          (CFML) is a scripting language that runs on the JVM and .NET
          framework. CFML augments HTML with syntax for database access, file
          manipulation, and other common web development tasks.

          ''
      , yearFirstPublished = 1995
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.cli ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "ColdFusion Homepage"
          , url = "https://www.adobe.com/products/coldfusion-family.html"
          }
        , { title = "ColdFusion Developers User Guide"
          , url = "https://helpx.adobe.com/coldfusion/cf-dev-user-guide.html"
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

          A standardized dialect of Lisp that unified several existing Lisp
          implementations. It is a multi-paradigm language featuring a macro
          system, dynamic typing, and support for multiple programming
          paradigms.

          ''
      , yearFirstPublished = 1984
      , compilationTargets =
        [ compilationTarget.machineCode, compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Common Lisp HyperSpec"
          , url = "http://www.lispworks.com/documentation/HyperSpec/Front"
          }
        , { title = "Common Lisp Wiki", url = "https://www.cliki.net" }
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
      , description =
          ''

          A programming language with Ruby-like syntax that compiles to native
          code. It aims to provide the productivity of Ruby with the performance
          and type safety of a compiled language.

          ''
      , yearFirstPublished = 2014
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Crystal Homepage", url = "https://crystal-lang.org" }
        , { title = "Crystal Documentation"
          , url = "https://crystal-lang.org/reference"
          }
        ]
      }
    , { name = "Curry"
      , originalAuthors = [ "Michael Hanus" ]
      , paradigms = [ paradigms.functional, paradigms.logic ]
      , examples = [] : List Text
      , description =
          ''

          A declarative programming language that combines functional
          programming with logic programming features. Named after Haskell
          B. Curry (also the namesake of the programming language Haskell), it
          integrates most of the important features of functional languages like
          Haskell and logic languages like Prolog.

          ''
      , yearFirstPublished = 1999
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Curry Homepage", url = "http://www.curry-lang.org" }
        , { title = "Curry Documentation"
          , url = "http://www.curry-lang.org/documentation"
          }
        ]
      }
    , { name = "D"
      , originalAuthors = [ "Walter Bright", "Andrei Alexandrescu" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples =
        [ ''
          import std.stdio, std.array, std.algorithm;

          void main()
          {
              stdin
                  .byLineCopy
                  .array
                  .sort!((a, b) => a > b) // descending order
                  .each!writeln;
          }
          ''
        ]
      , description =
          ''

          D is a general-purpose programming language which is intended to be
          similar to, but an improvement upon, C and C++. Unlike those
          languages, D supports automatic memory management via garbage
          collection as well as manual memory management.

          ''
      , yearFirstPublished = 2001
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "D Language Homepage", url = "https://dlang.org" }
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

          A client-optimized programming language developed by Google for
          building web, mobile, and desktop applications. Originally designed as
          a replacement for JavaScript, it now focuses on being a
          general-purpose language with strong tooling support. Dart is the
          language used by the Flutter application framework.

          ''
      , yearFirstPublished = 2011
      , compilationTargets = [ compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Dart Homepage", url = "https://dart.dev" }
        , { title = "Dart Documentation", url = "https://dart.dev/guides" }
        , { title = "Dartpad (Online Editor)", url = "https://dartpad.dev" }
        , { title = "Flutter", url = "https://flutter.dev" }
        ]
      }
    , { name = "Datalog"
      , originalAuthors = [ "Hervé Gallaire", "Jack Minker" ]
      , paradigms = [ paradigms.logic, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          A declarative logic programming language that is a subset of Prolog,
          but typically uses a bottom-up rather than top-down evaluation style.
          It is often used as a query language for deductive databases such as
          Datomic.

          ''
      , yearFirstPublished = 1977
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static ]
      , resources =
        [ { title = "Datalog Draft Specification"
          , url = "https://datalog-specs.info"
          }
        , { title = "Datomic Database", url = "https://datomic.com" }
        ]
      }
    , { name = "Delphi"
      , originalAuthors =
        [ "Borland Software Corporation"
        , "CodeGear"
        , "Embarcadero Technologies"
        , "Anders Hejlsberg"
        ]
      , paradigms = [ paradigms.objectOriented, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          A high-level, compiled, strongly typed language that evolved from
          Pascal. Delphi is known for its rapid application development (RAD)
          capabilities, particularly for Windows applications.

          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Delphi Homepage"
          , url = "https://www.embarcadero.com/products/delphi"
          }
        , { title = "Delphi Documentation"
          , url = "https://docwiki.embarcadero.com/RADStudio/en/Main_Page"
          }
        ]
      }
    , { name = "Dhall"
      , originalAuthors = [ "Gabriel Gonzalez" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          A programmable configuration language that is not Turing
          complete. Dhall aims to be a standardized configuration language that
          is guaranteed to terminate and is more expressive than JSON or
          YAML. Unlike those formats, Dhall can specify types for data which
          ensures that data being loaded by an application has a correct format.

          ''
      , yearFirstPublished = 2017
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Dhall Homepage", url = "https://dhall-lang.org" }
        , { title = "Dhall Documentation", url = "https://docs.dhall-lang.org" }
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

          An object-oriented programming language designed to promote software
          quality through the use of Design by Contract (DbC). Eiffel emphasizes
          readability, reusability, and reliability.

          ''
      , yearFirstPublished = 1986
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Eiffel Homepage", url = "https://www.eiffel.org" }
        , { title = "Eiffel Documentation", url = "https://docs.eiffel.org" }
        ]
      }
    , { name = "Elixir"
      , originalAuthors = [ "José Valim" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          A functional programming language with Ruby-like syntax that runs on
          the BEAM (the Erlang virtual machine). Elixir is commonly used with
          the web server library Phoenix. Elixir takes heavy inspiration from
          Erlang and Elixir applications frequently leverage Erlang libraries.

          ''
      , yearFirstPublished = 2011
      , compilationTargets = [ compilationTarget.beamBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Elixir Homepage", url = "https://elixir-lang.org" }
        , { title = "Elixir Documentation", url = "https://hexdocs.pm/elixir" }
        , { title = "Phoenix Framework"
          , url = "https://www.phoenixframework.org"
          }
        ]
      }
    , { name = "Elm"
      , originalAuthors = [ "Evan Czaplicki" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A functional programming language that compiles to JavaScript,
          specifically designed for building web browser-based user
          interfaces. Elm emphasizes simplicity, ease of use (including friendly
          error messages), and no runtime exceptions.

          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Elm Homepage", url = "https://elm-lang.org" }
        , { title = "Elm Guide", url = "https://guide.elm-lang.org" }
        , { title = "Elm Package Documentation"
          , url = "https://package.elm-lang.org"
          }
        ]
      }
    , { name = "Emacs Lisp"
      , originalAuthors = [ "Richard Stallman" ]
      , paradigms = [ paradigms.functional, paradigms.imperative ]
      , examples =
        [ ''
          (message "Hello, World!")
          ''
        , ''
          (defun fib (n)
            (if (< n 2)
                n
              (+ (fib (- n 1)) (fib (- n 2)))))
          ''
        ]
      , description =
          ''

          A dialect of the Lisp programming language used as a scripting language by the Emacs text editor. Emacs Lisp provides text manipulation capabilities and is used to extend and customize Emacs.

          ''
      , yearFirstPublished = 1985
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Emacs Lisp Introduction"
          , url =
              "https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html"
          }
        , { title = "Emacs Lisp Reference Manual"
          , url =
              "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html"
          }
        ]
      }
    , { name = "Erlang"
      , originalAuthors = [ "Joe Armstrong", "Robert Virding", "Mike Williams" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          A functional programming language developed at Ericsson, designed for
          building scalable distributed systems. Erlang is paired with the Open
          Telecom Platform (OTP), which provides many common abstractions for
          the language. Erlang is famous for its massive parallelism, it use of
          actor-model concurrency, and its "supervision tree" model of
          monitoring and restarting processes.

          ''
      , yearFirstPublished = 1986
      , compilationTargets = [ compilationTarget.beamBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Erlang Homepage", url = "https://www.erlang.org" }
        , { title = "Erlang Documentation"
          , url = "https://www.erlang.org/docs"
          }
        ]
      }
    , { name = "Euphoria"
      , originalAuthors = [ "Robert Craig" ]
      , paradigms = [ paradigms.imperative ]
      , examples =
        [ ''
          procedure Hello(sequence place)
            puts(1, "Hello, " & place & "!\n")
          end procedure

          Hello("World")
          ''
        ]
      , description =
          ''

          The Euphoria programming language strives to be simple, flexible, and
          easy-to-learn. It was designed to be easy to read and write, and
          prefers English keywords over symbols in its syntax.

          ''
      , yearFirstPublished = 1993
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Open Euphoria Homepage", url = "http://openeuphoria.org" }
        , { title = "Open Euphoria Manual"
          , url = "https://openeuphoria.org/docs"
          }
        , { title = "Rapide Euphoria Homepage"
          , url = "https://www.rapideuphoria.com"
          }
        , { title = "Rapide Euphoria Documentation"
          , url = "https://www.rapideuphoria.com/docs.htm"
          }
        ]
      }
    , { name = "F#"
      , originalAuthors = [ "Don Syme" ]
      , paradigms = [ paradigms.functional, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          A ML-like, functional-first programming language for the .NET
          ecosystem.

          ''
      , yearFirstPublished = 2005
      , compilationTargets = [ compilationTarget.cli ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "F# Homepage", url = "https://fsharp.org" }
        , { title = "F# Documentation"
          , url = "https://docs.microsoft.com/en-us/dotnet/fsharp"
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

          A stack-oriented programming language with high-level features like
          dynamic typing, extensible syntax, macros, and garbage
          collection. Factor emphasizes interactive development and
          concatenative programming.

          ''
      , yearFirstPublished = 2003
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Factor Homepage", url = "https://factorcode.org" }
        , { title = "Factor Documentation"
          , url = "https://docs.factorcode.org"
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

          A stack-based programming language emphasizing simplicity and
          extensibility.

          ''
      , yearFirstPublished = 1970
      , compilationTargets =
        [ compilationTarget.interpreted, compilationTarget.machineCode ]
      , typing = [ typingSystem.weak ]
      , resources =
        [ { title = "Forth Interest Group", url = "http://www.forth.org" } ]
      }
    , { name = "Fortran"
      , originalAuthors = [ "John Backus" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          The first high-level programming language, designed for scientific
          computing. While no longer popular, Fortran is still occasionally used
          in scientific computing.

          ''
      , yearFirstPublished = 1957
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Fortran Homepage", url = "https://fortran-lang.org" }
        , { title = "Fortran Documentation"
          , url = "https://fortran-lang.org/learn"
          }
        ]
      }
    , { name = "Gleam"
      , originalAuthors = [ "Louis Pilfold" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A statically typed functional programming language. Gleam's compiler
          is implemented in Rust, and the language has many syntactic and
          semantic similarities to Rust. Gleam can compile to BEAM (Erlang
          virtual machine) bytecode or JavaScript, and can call functions
          available in those environments (FFI).

          ''
      , yearFirstPublished = 2016
      , compilationTargets = [ compilationTarget.beamBytecode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "Gleam Homepage", url = "https://gleam.run" }
        , { title = "Gleam Documentation"
          , url = "https://gleam.run/documentation"
          }
        , { title = "Gleam Discord", url = "https://discord.gg/Fm8Pwmy" }
        ]
      }
    , { name = "Go"
      , originalAuthors = [ "Robert Griesemer", "Rob Pike", "Ken Thompson" ]
      , paradigms = [ paradigms.imperative ]
      , examples =
        [ ''
          package main

          import "fmt"

          func main() {
          	fmt.Println("Hello, 世界")
          }
          ''
        ]
      , description =
          ''

          A statically typed, compiled language designed at Google and strongly
          influenced by C. Go's design emphasizes simplicity and safety, for
          instance, Go features garbage collection instead of manual memory
          management. Go supports concurrency through coroutines (called
          "goroutines").

          ''
      , yearFirstPublished = 2009
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Go Homepage", url = "https://golang.org" }
        , { title = "Go Documentation", url = "https://golang.org/doc" }
        ]
      }
    , { name = "Groovy"
      , originalAuthors = [ "James Strachan" ]
      , paradigms =
        [ paradigms.objectOriented, paradigms.functional, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          An object-oriented programming language for the Java platform. Most
          valid Java files are also valid Groovy files, allowing Java developers
          to easily pick up the language. Groovy features more concise syntax
          than Java for certain operations, as well as dynamic typing, operator
          overloading, and more.

          ''
      , yearFirstPublished = 2003
      , compilationTargets = [ compilationTarget.jvmBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Groovy Homepage", url = "https://groovy-lang.org" }
        , { title = "Groovy Documentation"
          , url = "https://groovy-lang.org/documentation.html"
          }
        ]
      }
    , { name = "Guile"
      , originalAuthors =
        [ "Aubrey Jaffer", "Tom Lord", "Miles Bader", "Jim Blandy" ]
      , paradigms = [ paradigms.functional, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          GNU Ubiquitous Intelligent Language for Extensions (Guile) is an
          implementation of the Scheme programming language, designed to be
          embedded in other applications to provide scripting and extension
          capabilities.

          ''
      , yearFirstPublished = 1993
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Guile Homepage"
          , url = "https://www.gnu.org/software/guile"
          }
        , { title = "Guile Documentation"
          , url = "https://www.gnu.org/software/guile/docs"
          }
        ]
      }
    , { name = "Hack"
      , originalAuthors =
        [ "Julien Verlaguet", "Alok Menghrajani", "Drew Paroski" ]
      , paradigms =
        [ paradigms.imperative, paradigms.functional, paradigms.objectOriented ]
      , examples =
        [ ''
          use namespace HH\Lib\IO;

          <<__EntryPoint>>
          async function main(): Awaitable<void> {
            await IO\request_output()->writeAllAsync("Hello World!\n");
          }
          ''
        ]
      , description =
          ''

          Hack was developed at Facebook as a derivative of PHP. Much PHP code
          is compatible with Hack. Hack runs on the HipHop Virtual Machine
          (HHVM), also developed at Facebook.

          ''
      , yearFirstPublished = 2014
      , typing = [ typingSystem.gradual ]
      , compilationTargets = [ compilationTarget.interpreted ]
      , resources = [ { title = "Hack Website", url = "https://hacklang.org" } ]
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
      , description =
          ''

          A purely functional programming language with strong static typing and
          lazy evaluation. Haskell is commonly the foundation for functional
          programming language research.

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
        [ { title = "Haskell Homepage", url = "https://www.haskell.org" }
        , { title = "Haskell Documentation"
          , url = "https://www.haskell.org/documentation"
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
      , description =
          ''

          A general-purpose functional programming language with dependent
          types, which allows types to be predicated on values. Idris aims to
          provide a practical programming language with full dependent types.

          ''
      , yearFirstPublished = 2013
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.dependent ]
      , resources =
        [ { title = "Idris Homepage", url = "https://www.idris-lang.org" }
        , { title = "Idris Documentation"
          , url = "https://idris2.readthedocs.io/en/latest"
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

          A pure object-oriented programming language inspired by Smalltalk,
          Self, and Lisp. Everything in Io is a message that is passed to
          objects. It features a small core with highly dynamic and reflective
          capabilities.

          ''
      , yearFirstPublished = 2002
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Io Language", url = "https://iolanguage.org" }
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

          A prototype-based, dynamic programming language inspired by Io,
          Smalltalk, Ruby, and Lisp. It runs on the Java Virtual Machine and
          emphasizes expressiveness and experimentation.

          ''
      , yearFirstPublished = 2008
      , compilationTargets = [ compilationTarget.jvmBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Ioke Language", url = "https://ioke.org" }
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

          A successor to APL focusing on array programming using only the ASCII
          character set.

          ''
      , yearFirstPublished = 1990
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "J Homepage", url = "https://www.jsoftware.com" }
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

          A class-based, object-oriented programming language designed to be
          'write once, run anywhere' via the Java Virtual Machine. Originally
          designed at Sun Microsystems, Java is currently owned by Oracle. Java
          is one of the most widely-used programming languages today.

          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.jvmBytecode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.nominal ]
      , resources =
        [ { title = "Java Homepage", url = "https://www.java.com" }
        , { title = "Java Documentation"
          , url = "https://docs.oracle.com/en/java"
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
        , ''
          function addOne(x) {
            return x + 1;
          }
          ''
        , ''
          const addOne = (x) => x + 1;
          ''
        ]
      , description =
          ''

          A high-level, multi-paradigm programming language that powers most
          interactive websites. Originally created for Netscape Navigator, it is
          now implemented in all mainstream web browsers. JavaScript can also be
          used outside of the browser with runtimes such as Node.js, Deno, or,
          Bun.

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
              "https://www.ecma-international.org/publications-and-standards/standards/ecma-262"
          }
        ]
      }
    , { name = "Julia"
      , originalAuthors =
        [ "Jeff Bezanson", "Stefan Karpinski", "Viral B. Shah", "Alan Edelman" ]
      , paradigms = [ paradigms.functional, paradigms.imperative ]
      , examples = [ "println(\"Hello, World!\")" ]
      , description =
          ''

          A high-performance, general-purpose programming language, with syntax
          that is familiar to users of scripting languages. Julia provides a
          sophisticated compiler, distributed parallel execution, numerical
          accuracy, and an extensive mathematical function library.

          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.dynamic
        , typingSystem.inferred
        , typingSystem.nominal
        , typingSystem.strong
        ]
      , resources =
        [ { title = "Julia Homepage", url = "https://julialang.org" }
        , { title = "Julia Documentation", url = "https://docs.julialang.org" }
        , { title = "Julia GitHub Repository"
          , url = "https://github.com/JuliaLang/julia"
          }
        ]
      }
    , { name = "K"
      , originalAuthors = [ "Arthur Whitney" ]
      , paradigms = [ paradigms.array, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          An array programming language used primarily in financial
          applications.

          ''
      , yearFirstPublished = 1993
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources = [ { title = "Kx Systems", url = "https://kx.com" } ]
      }
    , { name = "Koka"
      , originalAuthors = [ "Daan Leijen" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A strongly-typed functional programming language with a focus on
          effect systems and algebraic effects. Koka aims to provide a clear
          separation between pure and effectful computations.

          ''
      , yearFirstPublished = 2014
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "Koka Homepage", url = "https://koka-lang.github.io/koka" }
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

          A modern programming language developed by IDE vendor JetBrains
          targeting the JVM, Android, and web browsers (via compilation to
          JavaScript). Kotlin is the default for new Android apps in the
          official Android Sutdio IDE (also made by JetBrains).

          ''
      , yearFirstPublished = 2011
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Kotlin Homepage", url = "https://kotlinlang.org" }
        , { title = "Kotlin Documentation"
          , url = "https://kotlinlang.org/docs"
          }
        ]
      }
    , { name = "Lisp"
      , originalAuthors = [ "John McCarthy", "Steve Russell" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          The original LISt Processing language, designed for symbolic
          computation and known for its simple, yet powerful, syntax based on
          S-expressions. Lisp has influenced many other programming languages
          and has numerous dialects, including Common Lisp and Scheme.

          ''
      , yearFirstPublished = 1958
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Wikipedia: Lisp (programming language)"
          , url = "https://en.wikipedia.org/wiki/Lisp_(programming_language)"
          }
        ]
      }
    , { name = "Lisp Flavored Erlang (LFE)"
      , originalAuthors = [ "Robert Virding" ]
      , paradigms = [ paradigms.functional, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          A Lisp syntax front-end to BEAM (the Erlang virtual machine). LFE combines the power of
          Lisp with the robustness and concurrency of the Erlang ecosystem.

          ''
      , yearFirstPublished = 2007
      , compilationTargets = [ compilationTarget.beamBytecode ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "LFE Homepage", url = "https://lfe.io" }
        , { title = "LFE Documentation", url = "https://docs.lfe.io" }
        , { title = "LFE GitHub Repository"
          , url = "https://github.com/lfe/lfe"
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

          A lightweight, high-level scripting language designed primarily for
          embedded use in applications. Known for its efficiency, portability,
          and ease of integration.

          ''
      , yearFirstPublished = 1993
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Lua Homepage", url = "https://www.lua.org" }
        , { title = "Lua Documentation", url = "https://www.lua.org/docs.html" }
        ]
      }
    , { name = "MATLAB"
      , originalAuthors = [ "Cleve Moler" ]
      , paradigms = [ paradigms.array, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          Originally \"MATrix LABoratory\", MATLAB is a widely-used,
          closed-source numerical computing environment and programming
          language.

          ''
      , yearFirstPublished = 1984
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "MATLAB Homepage"
          , url = "https://www.mathworks.com/products/matlab.html"
          }
        , { title = "MATLAB Documentation"
          , url = "https://www.mathworks.com/help/matlab"
          }
        ]
      }
    , { name = "Magik"
      , originalAuthors = [ "Arthur Chance" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          A dynamic object-oriented programming language originally developed by
          Smallworld for developing geographical information systems (GIS), now
          offered by GE Energy. Features multiple inheritance, dynamic typing,
          and garbage collection.

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

          A logic/functional programming language with a Prolog-like syntax,
          featuring advanced static analysis and error detection
          features. Mercury improves upon Prolog with a strong type system, mode
          system, and determinism analysis.

          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Mercury Homepage", url = "https://mercurylang.org" }
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

          A successor to Pascal emphasizing modularity, wherein groups of
          related declarations are grouped into modules.

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

          A successor to Modula with improved type safety and module
          system. Modula-2 added support for separate compilation, co-routines
          for concurrent programming, and a more sophisticated module system. It
          influenced later languages like Ada and Oberon.

          ''
      , yearFirstPublished = 1978
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ADW Modula-2 Homepage", url = "http://www.modula2.org" } ]
      }
    , { name = "Nim"
      , originalAuthors = [ "Andreas Rumpf" ]
      , paradigms =
        [ paradigms.imperative, paradigms.objectOriented, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          Nim (originally Nimrod) is a statically typed compiled systems
          programming language that combines the performance and control of C
          with a Python-like syntax. Nim is designed to be efficient,
          expressive, and elegant, with a focus on metaprogramming and
          compile-time execution.

          ''
      , yearFirstPublished = 2008
      , compilationTargets =
        [ compilationTarget.machineCode, compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Nim Homepage", url = "https://nim-lang.org" }
        , { title = "Nim Documentation"
          , url = "https://nim-lang.org/documentation.html"
          }
        , { title = "Nim GitHub Repository"
          , url = "https://github.com/nim-lang/Nim"
          }
        ]
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

          Formerly Objective Caml, OCaml is a general-purpose programming
          language in the ML family that supports functional, imperative, and
          object-oriented programming styles. OCaml emphasizes type safety and
          expressiveness while maintaining high performance through native code
          compilation.

          ''
      , yearFirstPublished = 1996
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "OCaml Homepage", url = "https://ocaml.org" }
        , { title = "OCaml Documentation", url = "https://ocaml.org/docs" }
        , { title = "Real World OCaml (book)"
          , url = "https://dev.realworldocaml.org"
          }
        ]
      }
    , { name = "Oberon"
      , originalAuthors = [ "Niklaus Wirth" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          A successor to Modula-2 that introduced object-oriented features while
          maintaining simplicity and efficiency. It was designed alongside the
          Oberon operating system.

          ''
      , yearFirstPublished = 1987
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ETH Oberon"
          , url = "https://www.inf.ethz.ch/personal/wirth/Oberon"
          }
        ]
      }
    , { name = "Object Pascal"
      , originalAuthors =
        [ "Larry Tesler", "Niklaus Wirth", "Anders Hejlsberg" ]
      , paradigms = [ paradigms.objectOriented, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          An extension of the Pascal programming language (by way of the obscure
          language Clascal) that supports object-oriented programming. It was
          developed by Apple Computer with the help of Niklaus Wirth and later
          integrated by Borland into Turbo Pascal to become the basis of their
          Delphi product.

          ''
      , yearFirstPublished = 1986
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Free Pascal (an Object Pascal compiler) Documentation"
          , url = "https://www.freepascal.org/docs.html"
          }
        ]
      }
    , { name = "Objective-C"
      , originalAuthors = [ "Brad Cox", "Tom Love" ]
      , paradigms = [ paradigms.objectOriented, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          A general-purpose, object-oriented programming language that adds
          Smalltalk-style messaging to the C programming language. It was the
          main programming language used by Apple for macOS and iOS development
          before the introduction of Swift.

          ''
      , yearFirstPublished = 1984
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Objective-C Documentation"
          , url = "https://developer.apple.com/documentation/objectivec"
          }
        , { title = "Objective-C Programming Guide"
          , url =
              "https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html"
          }
        ]
      }
    , { name = "Octave"
      , originalAuthors = [ "John W. Eaton" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          A high-level language, primarily used for numerical computations.
          Octave is a free and open-source alternative to MATLAB.

          ''
      , yearFirstPublished = 1992
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "GNU Octave", url = "https://www.gnu.org/software/octave" }
        , { title = "Octave Documentation"
          , url = "https://octave.org/doc/interpreter"
          }
        ]
      }
    , { name = "PHP"
      , originalAuthors = [ "Rasmus Lerdorf" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          A server-side scripting language originally designed for web
          development. PHP originally stood for "Personal Home Page" but was
          later renamed to "PHP: Hypertext Preprocessor".

          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.weak ]
      , resources =
        [ { title = "PHP Homepage", url = "https://www.php.net" }
        , { title = "PHP Documentation", url = "https://www.php.net/docs.php" }
        ]
      }
    , { name = "PL/I"
      , originalAuthors = [ "IBM" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          Programming Language One, designed by IBM and its user group, SHARE,
          to succeed FORTRAN and COBOL.

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

          A language designed to encourage good programming practices.  Wirth's
          efforts were originally part of the effort to design the next version
          of ALGOL (the ALGOL X effort), but Wirth's proposals were not
          accepted, so he created Pascal instead.  Pascal was very popular in
          the 1970s and 1980s, but has since been largely supplanted by C and
          C++.

          ''
      , yearFirstPublished = 1970
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Wikipedia: Pascal"
          , url = "https://en.wikipedia.org/wiki/Pascal_(programming_language)"
          }
        , { title = "Free Pascal (a Pascal compiler)"
          , url = "https://www.freepascal.org"
          }
        ]
      }
    , { name = "Perl"
      , originalAuthors = [ "Larry Wall" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          A general-purpose language originally designed for text processing.
          Perl is known for its flexibility and its ability to easily create
          domain-specific languages.

          ''
      , yearFirstPublished = 1987
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Perl Homepage", url = "https://www.perl.org" }
        , { title = "Perl Documentation", url = "https://perldoc.perl.org" }
        ]
      }
    , { name = "Pony"
      , originalAuthors = [ "Sebastian Blessing", "Sylvan Clebsch" ]
      , paradigms = [ paradigms.objectOriented, paradigms.functional ]
      , examples =
        [ ''
          actor Main
            new create(env: Env) =>
              env.out.print("Hello, world!")
          ''
        ]
      , description =
          ''

          An object-oriented programming language designed for writing safe,
          high-performance actor-based programs. Pony emphasizes
          capabilities-secure type system and data-race freedom through its
          reference capabilities system.

          ''
      , yearFirstPublished = 2015
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.nominal ]
      , resources =
        [ { title = "Pony Homepage", url = "https://www.ponylang.io" }
        , { title = "Pony Tutorial", url = "https://tutorial.ponylang.io" }
        , { title = "Pony Documentation"
          , url = "https://www.ponylang.io/learn"
          }
        ]
      }
    , { name = "Prolog"
      , originalAuthors = [ "Alain Colmerauer", "Philippe Roussel" ]
      , paradigms = [ paradigms.logic, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          A logic programming language based on first-order predicate
          calculus. Programs consist of facts and rules, and computation
          proceeds by making logical queries against this knowledge base.

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
          , url = "http://gprolog.org"
          }
        ]
      }
    , { name = "PureScript"
      , originalAuthors = [ "Phil Freeman" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A strongly-typed functional programming language that compiles to
          JavaScript. PureScript is similar to Haskell but designed specifically
          for the web platform, featuring row polymorphism, type classes, and
          strict evaluation.

          ''
      , yearFirstPublished = 2013
      , compilationTargets = [ compilationTarget.javascript ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.inferred ]
      , resources =
        [ { title = "PureScript Homepage", url = "https://www.purescript.org" }
        , { title = "PureScript Documentation"
          , url = "https://github.com/purescript/documentation"
          }
        , { title = "Try PureScript", url = "https://try.purescript.org" }
        ]
      }
    , { name = "Pyret"
      , originalAuthors =
        [ "Joe Gibbs Politz"
        , "Benjamin Lerner"
        , "Daniel Patterson"
        , "Dorai Sitaram"
        ]
      , paradigms = [ paradigms.functional, paradigms.objectOriented ]
      , examples =
        [ ''
          print("Hello, World!")
          ''
        ]
      , description =
          ''

          A programming language designed for teaching computer science,
          featuring built-in testing, tables, and image manipulation. Pyret
          emphasizes clear error messages and integrates features from both
          functional and object-oriented programming.

          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Pyret Homepage", url = "https://www.pyret.org" }
        , { title = "Pyret Documentation", url = "https://www.pyret.org/docs" }
        , { title = "Online Editor", url = "https://code.pyret.org" }
        ]
      }
    , { name = "Python"
      , originalAuthors = [ "Guido van Rossum" ]
      , paradigms =
        [ paradigms.imperative, paradigms.objectOriented, paradigms.functional ]
      , examples =
        [ ''
          print("Hello, World!")
          ''
        ]
      , description =
          ''

          A high-level, general-purpose programming language emphasizing code
          readability with its notable use of significant whitespace. Python
          supports multiple programming paradigms and features a comprehensive
          (frequently referred to as "batteries-included") standard library.

          ''
      , yearFirstPublished = 1991
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Python Homepage", url = "https://www.python.org" }
        , { title = "Python Documentation", url = "https://docs.python.org" }
        ]
      }
    , { name = "QBasic"
      , originalAuthors = [ "Microsoft" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          An IDE and interpreter for a version of BASIC developed by
          Microsoft. It was included with MS-DOS and was many programmers' first
          introduction to programming.

          ''
      , yearFirstPublished = 1991
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "QB64 (a modern implementation of QBASIC)"
          , url = "https://qb64.com"
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
        [ { title = "R Homepage", url = "https://www.r-project.org" }
        , { title = "R Documentation", url = "https://www.rdocumentation.org" }
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

          A general-purpose, multi-paradigm programming language in the
          Lisp/Scheme family. Racket is designed to be a platform for
          programming language design, implementation, and learning.

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

          A multi-paradigm language formerly known as Perl 6, designed to be
          more consistent and modern than Perl 5.

          ''
      , yearFirstPublished = 2015
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing =
        [ typingSystem.dynamic, typingSystem.gradual, typingSystem.strong ]
      , resources =
        [ { title = "Raku Homepage", url = "https://raku.org" }
        , { title = "Raku Documentation", url = "https://docs.raku.org" }
        ]
      }
    , { name = "ReasonML"
      , originalAuthors = [ "Jordan Walke" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A syntax and toolchain for OCaml, created at Facebook. ReasonML
          provides a more familiar C-style syntax while leveraging OCaml's type
          system and compilation.

          ''
      , yearFirstPublished = 2016
      , compilationTargets =
        [ compilationTarget.machineCode, compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "ReasonML Homepage", url = "https://reasonml.github.io" }
        , { title = "ReasonML Documentation"
          , url = "https://reasonml.github.io/docs/en/what-and-why"
          }
        , { title = "Try ReasonML Online"
          , url = "https://reasonml.github.io/en/try"
          }
        , { title = "ReasonML GitHub Repository"
          , url = "https://github.com/reasonml/reason"
          }
        ]
      }
    , { name = "Rebol"
      , originalAuthors = [ "Carl Sassenrath" ]
      , paradigms = [ paradigms.functional, paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          A lightweight messaging language designed for distributed computing
          and network communications, with a focus on human-readable syntax.
          Its author, Carl Sassenrath, claims its greatest strength is its
          ability to easily create domain-specific languages.

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

          A next-generation programming language strongly inspired by Rebol but
          with a focus on cross-compilation, concurrency and high performance.

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
    , { name = "Roc"
      , originalAuthors = [ "Richard Feldman" ]
      , paradigms = [ paradigms.functional ]
      , examples =
        [ ''
          credits = List.map(songs, |song|
              "Performed by ''${song.artist}"
          )
          ''
        ]
      , description =
          ''

          A fast, friendly, functional programming language focusing on
          zero-cost abstractions, great error messages, and first-class support
          for building command-line interfaces and network services.

          ''
      , yearFirstPublished = 2020
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Roc Homepage", url = "https://www.roc-lang.org" }
        , { title = "Roc Documentation"
          , url = "https://www.roc-lang.org/tutorial"
          }
        ]
      }
    , { name = "Ruby"
      , originalAuthors = [ "Yukihiro \"Matz\" Matsumoto" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [ "puts \"Hello World!\"" ]
      , description =
          ''

          A dynamic, object-oriented language emphasizing simplicity and
          productivity. Ruby is widely known for the web development framework
          Ruby on Rails, upon which many successful companies (Twitter, GitHub)
          were built.

          ''
      , yearFirstPublished = 1995
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Ruby Homepage", url = "https://ruby-lang.org" }
        , { title = "Ruby Documentation"
          , url = "https://www.ruby-lang.org/en/documentation"
          }
        , { title = "Try Ruby (browser REPL)"
          , url = "https://try.ruby-lang.org"
          }
        ]
      }
    , { name = "Rust"
      , originalAuthors = [ "Graydon Hoare" ]
      , paradigms = [ paradigms.imperative, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A systems programming language focused on safety, concurrency, and
          performance, originally developed at Mozilla. Rust is notable for its
          compile-time memory safety guarantees through a unique ownership
          system.

          ''
      , yearFirstPublished = 2012
      , compilationTargets =
        [ compilationTarget.machineCode, compilationTarget.webassembly ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Rust Homepage", url = "https://www.rust-lang.org" }
        , { title = "Rust Documentation", url = "https://doc.rust-lang.org" }
        , { title = "The Rust Programming Language (book)"
          , url = "https://doc.rust-lang.org/book"
          }
        ]
      }
    , { name = "SNOBOL"
      , originalAuthors = [ "Ralph Griswold", "Ivan Polonsky", "David Farber" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          StriNg Oriented symBOlic Language, designed for text processing.
          SNOBOL had support for "patterns", which were more powerful than
          regular expressions. SNOBOL was used for text processing in the 1960s
          and 1970s, but was largely supplanted by Perl and AWK.

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

          Structured Query Language, a domain-specific language designed for
          managing and querying relational databases. It has become the standard
          language for relational database management systems, used in
          PostgreSQL, MySQL, Oracle, and other database systems.

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

          Originally SCAlable LAnguage, Scala is a language combining
          object-oriented and functional programming running on the JVM.

          ''
      , yearFirstPublished = 2004
      , compilationTargets =
        [ compilationTarget.jvmBytecode, compilationTarget.javascript ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Scala Homepage", url = "https://www.scala-lang.org" }
        , { title = "Scala Documentation", url = "https://docs.scala-lang.org" }
        ]
      }
    , { name = "Scheme"
      , originalAuthors = [ "Guy L. Steele", "Gerald Jay Sussman" ]
      , paradigms = [ paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A dialect of Lisp emphasizing simplicity and minimalism, widely used
          in computer science education and research. There are several
          specifications for the Scheme language, the latest of which is R7RS
          for which the "small language" design was completed in 2013, but the
          "large language" design is unfinished.

          ''
      , yearFirstPublished = 1975
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Wikipedia: Scheme (programming language)"
          , url = "https://en.wikipedia.org/wiki/Scheme_(programming_language)"
          }
        , { title = "R7RS Website", url = "https://r7rs.org" }
        ]
      }
    , { name = "Scratch"
      , originalAuthors = [ "Mitchel Resnick" ]
      , paradigms = [ paradigms.imperative, paradigms.declarative ]
      , examples = [] : List Text
      , description =
          ''

          A block-based visual programming language and online community
          targeted primarily at children. Scratch allows users to create
          programs by snapping together code blocks in a drag-and-drop
          interface.

          ''
      , yearFirstPublished = 2003
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Scratch Homepage", url = "https://scratch.mit.edu" } ]
      }
    , { name = "Self"
      , originalAuthors = [ "David Ungar", "Randall Smith" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          Self is a prototype-based object-oriented programming language that
          pioneered many concepts used in modern languages like JavaScript. Self
          was based on Smalltalk, but designed to further object-oriented
          programming language research. It was originally designed at Xerox
          PARC, but most of Self's development was done at Sun Microsystems.

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

          Developed at Xerox PARC, Smalltalk is one of the first pure
          object-oriented programming languages, where all data, even basic
          types like numbers, are objects. Smalltalk was very influential in the
          design of later object-oriented programming languages.

          ''
      , yearFirstPublished = 1972
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic, typingSystem.strong ]
      , resources =
        [ { title = "Squeak Homepage (Smalltalk Implementation)"
          , url = "https://squeak.org"
          }
        , { title = "Pharo Homepage (Smalltalk Implementation)"
          , url = "https://pharo.org"
          }
        ]
      }
    , { name = "Simula"
      , originalAuthors = [ "Ole-Johan Dahl", "Kristen Nygaard" ]
      , paradigms = [ paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          A simulation programming language, Simula is considered the first
          object-oriented programming language. It is an approximate superset of
          ALGOL 60.

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

          An attempt to standardize ML implementations, Standard ML (SML) is a
          general-purpose functional programming language that features static
          typing, type inference, pattern matching, and a sophisticated module
          system. SML influenced many modern functional languages including
          OCaml, F#, and Scala.

          ''
      , yearFirstPublished = 1983
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Standard ML Family GitHub"
          , url = "https://smlfamily.github.io"
          }
        , { title = "SML/NJ (a compiler for SML)"
          , url = "https://www.smlnj.org"
          }
        , { title = "MLton (a compiler for SML)", url = "http://mlton.org" }
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

          A modern programming language developed by Apple as a replacement for
          Objective-C. Swift is primarily used to create apps for iOS, macOS,
          and other Apple operating systems.

          ''
      , yearFirstPublished = 2014
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Swift Homepage", url = "https://www.swift.org" }
        , { title = "Swift Documentation", url = "https://docs.swift.org" }
        , { title = "The Swift Programming Language (book)"
          , url =
              "https://docs.swift.org/swift-book/documentation/the-swift-programming-language"
          }
        , { title = "Apple Documentation"
          , url = "https://developer.apple.com/swift"
          }
        ]
      }
    , { name = "Tcl"
      , originalAuthors = [ "John Ousterhout" ]
      , paradigms = [ paradigms.imperative ]
      , examples =
        [ ''
          puts "Hello, World!"
          ''
        , ''
          set sum [expr 1+2+3+4+5]
          puts "The sum of the numbers 1..5 is $sum."
          ''
        ]
      , description =
          ''

          Tool Command Language, a scripting language commonly used for rapid
          prototyping, scripted applications, GUIs, and testing.  Tcl, often
          pronounced "tickle", has a unique typing system in which all data may
          be manipulated as strings. Tcl is often paired with Tk, a GUI toolkit.

          ''
      , yearFirstPublished = 1988
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.dynamic ]
      , resources =
        [ { title = "Tcl Homepage", url = "https://www.tcl-lang.org" }
        , { title = "Tcl Documentation", url = "https://www.tcl-lang.org/doc" }
        ]
      }
    , { name = "Turbo Pascal"
      , originalAuthors = [ "Borland Software Corporation", "Anders Hejlsberg" ]
      , paradigms = [ paradigms.imperative ]
      , examples = [] : List Text
      , description =
          ''

          A software development system that includes a compiler and an
          integrated development environment (IDE) for the Pascal programming
          language on CP/M, CP/M-86, and MS-DOS. Turbo Pascal was developed by
          Borland and was popular in the 1980s and early 1990s.

          ''
      , yearFirstPublished = 1983
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Wikipedia: Turbo Pascal"
          , url = "https://en.wikipedia.org/wiki/Turbo_Pascal"
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

          A strict syntactical superset of JavaScript that adds optional static
          typing. Developed by Microsoft to enable JavaScript development at
          scale. TypeScript has wide support in modern browser application
          development.

          ''
      , yearFirstPublished = 2012
      , compilationTargets = [ compilationTarget.javascript ]
      , typing =
        [ typingSystem.static, typingSystem.strong, typingSystem.structural ]
      , resources =
        [ { title = "TypeScript Homepage"
          , url = "https://www.typescriptlang.org"
          }
        , { title = "TypeScript Documentation"
          , url = "https://www.typescriptlang.org/docs"
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

          A modern functional programming language that emphasizes distributed
          computing and features a unique content-addressed codebase. Unison
          aims to simplify code collaboration and versioning.

          ''
      , yearFirstPublished = 2019
      , compilationTargets = [ compilationTarget.interpreted ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "Unison Homepage", url = "https://www.unison-lang.org" }
        , { title = "Unison Documentation"
          , url = "https://www.unison-lang.org/docs"
          }
        , { title = "Unison Discord", url = "https://unison-lang.org/discord" }
        , { title = "Unison Blog", url = "https://www.unison-lang.org/blog" }
        , { title = "Unison Cloud", url = "https://www.unison.cloud" }
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

          A systems programming language, very similar to Go. V emphasizes
          safety, performance, and simplicity with fast compilation, memory
          safety without garbage collection, and C-style syntax.

          ''
      , yearFirstPublished = 2019
      , compilationTargets = [ compilationTarget.machineCode ]
      , typing = [ typingSystem.static, typingSystem.strong ]
      , resources =
        [ { title = "V Programming Language", url = "https://vlang.io" }
        , { title = "V Documentation", url = "https://docs.vlang.io" }
        , { title = "V GitHub Repository", url = "https://github.com/vlang/v" }
        , { title = "V Playground (online editor)"
          , url = "https://play.vlang.io"
          }
        ]
      }
    , { name = "Visual Basic"
      , originalAuthors = [ "Microsoft" ]
      , paradigms = [ paradigms.imperative, paradigms.objectOriented ]
      , examples = [] : List Text
      , description =
          ''

          A version of BASIC developed by Microsoft for its COM programming
          model and Windows GUI development. The latest version is Visual Basic
          .NET.

          ''
      , yearFirstPublished = 1991
      , compilationTargets =
        [ compilationTarget.machineCode, compilationTarget.cli ]
      , typing = [ typingSystem.static ]
      , resources =
        [ { title = "Visual Basic Documentation"
          , url = "https://learn.microsoft.com/en-us/dotnet/visual-basic"
          }
        ]
      }
    , { name = "Zig"
      , originalAuthors = [ "Andrew Kelley" ]
      , paradigms = [ paradigms.imperative, paradigms.functional ]
      , examples = [] : List Text
      , description =
          ''

          A general-purpose programming language with an emphasis on being
          explicit. To this end, Zig makes features that are usually implicit,
          such as memory allocations, explicit. Zig supports incremental
          adoption into C/C++ codebases.

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
