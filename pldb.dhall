let Paradigm =
      < imperative
      | declarative
      | functional
      | array
      | objectOriented
      | logic
      | stack
      >

let CompilationTarget =
      < machineCode
      | jvmBytecode
      | beamBytecode
      | cil
      | interpreted
      | webassembly
      | language : { name : Text }
      >

let TypingSystem =
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

let ExampleType =
      < helloWorld
      | vector
      | fibonacci
      | factorial
      | quicksort
      | average
      | sum
      | map
      >

let Example = { type : ExampleType, content : Text }

let ResourceType =
      < book
      | documentation
      | homepage
      | implementation
      | languageReference
      | languageSpecification
      | playground
      | popularLibrary
      | notableApplication
      | sourceRepository
      | tutorial
      | wikipediaEntry
      >

let Resource = { title : Text, type : ResourceType, href : Text }

let Language =
      { name : Text
      , originalAuthors : List Text
      , paradigms : List Paradigm
      , examples : List Example
      , description : Text
      , yearFirstPublished : Natural
      , compilationTargets : List CompilationTarget
      , typing : List TypingSystem
      , resources : List Resource
      }

in    [ { name = "8th"
        , originalAuthors = [ "Ron Nicholson" ]
        , paradigms = [ Paradigm.stack, Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                Hello, world!\n" . bye
                ''
            }
          ]
        , description =
            ''

            A Forth-like, cross-platform programming language focused on mobile
            and desktop development. 8th is a commercial product, but offers a
            free version.

            ''
        , yearFirstPublished = 2012
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.strong ]
        , resources =
          [ { title = "8th Homepage"
            , type = ResourceType.homepage
            , href = "https://8th-dev.com"
            }
          , { title = "8th Documentation"
            , type = ResourceType.documentation
            , href = "https://8th-dev.com/manual.html"
            }
          ]
        }
      , { name = "ABAP"
        , originalAuthors = [ "SAP SE" ]
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            Advanced Business Application Programming, a high-level programming
            language created by SAP for developing business applications in the
            SAP environment.

            ''
        , yearFirstPublished = 1983
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "ABAP Documentation"
            , type = ResourceType.documentation
            , href =
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
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            Originally termed International Algebraic Language (IAL), ALGOL 58 was
            an attempt to create a universal programming language.  Consequently,
            the Communications of the ACM (CACM) used ALGOL notation to publish
            algorithms.  ALGOL 58 was rapidly succeeded by ALGOL 60.

            ''
        , yearFirstPublished = 1958
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources = [] : List Resource
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
          , "Adriaan \"Aad\" van Wijngaarden"
          , "Bernard Vauquois"
          , "Joseph Wegstein"
          , "Michael Woodger"
          ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            The ALGOrithmic Language 1960, a further development upon ALGOL 1958.

            ''
        , yearFirstPublished = 1960
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "ALGOL 60 Report"
            , type = ResourceType.languageSpecification
            , href = "https://www.masswerk.at/algol60/report.htm"
            }
          ]
        }
      , { name = "ALGOL 68"
        , originalAuthors =
          [ "Adriaan \"Aad\" van Wijngaarden"
          , "Barry Mailloux"
          , "John E. L. Peck"
          , "Cornelis H. A. Koster"
          ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            The ALGOrithmic Language 1968, a further development upon ALGOL 1960.

            ''
        , yearFirstPublished = 1968
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "ALGOL 68 Genie"
            , type = ResourceType.implementation
            , href = "https://jmvdveer.home.xs4all.nl/algol.html"
            }
          ]
        }
      , { name = "APL"
        , originalAuthors = [ "Kenneth Iverson" ]
        , paradigms = [ Paradigm.declarative, Paradigm.array ]
        , examples =
          [ { type = ExampleType.sum
            , content =
                ''
                sum←+/
                sum 10 20 30
                ''
            }
          , { type = ExampleType.average
            , content =
                ''
                Avg←{(+⌿⍵)÷≢⍵}
                Avg 1 6 3 4
                ''
            }
          ]
        , description =
            ''

            A Programming Language, originally a math notation, is an language
            specialized for manipulating multidimensional arrays. APL uses a
            non-ASCII character set, and historically was programmed using special
            keyboards that provided APL symbols.

            ''
        , yearFirstPublished = 1966
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "Dyalog APL"
            , type = ResourceType.implementation
            , href = "https://www.dyalog.com"
            }
          , { title = "TryAPL"
            , type = ResourceType.playground
            , href = "https://tryapl.org"
            }
          ]
        }
      , { name = "AWK"
        , originalAuthors =
          [ "Alfred Aho", "Peter Weinberger", "Brian Kernighan" ]
        , paradigms = [ Paradigm.declarative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                BEGIN { print "Hello, world!" }
                ''
            }
          ]
        , description =
            ''

            A programming language designed for text processing. Named after its
            authors (Aho, Weinberger, Kernighan), AWK is based on the model of matching a pattern in input data
            to a corresponding action.

            ''
        , yearFirstPublished = 1977
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "GAWK (GNU AWK) Documentation"
            , type = ResourceType.documentation
            , href = "https://www.gnu.org/software/gawk/manual/gawk.html"
            }
          ]
        }
      , { name = "Ada"
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , originalAuthors = [ "Jean Ichbiah et al" ]
        , examples = [] : List Example
        , description =
            ''

            A programming language commissioned by the United States Department of
            Defense (DoD) to supersede the variety of languages in use at the
            time. Originally designed for embedded systems, Ada emphasizes safety
            and security through strong typing, explicit concurrency, and
            protected objects.  Ada is named for Ada Lovelace.

            ''
        , yearFirstPublished = 1980
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Ada Information Clearinghouse"
            , type = ResourceType.homepage
            , href = "https://www.adaic.org"
            }
          , { title = "Learn Ada"
            , type = ResourceType.tutorial
            , href = "https://learn.adacore.com"
            }
          ]
        }
      , { name = "Agda"
        , originalAuthors = [ "Ulf Norell", "Catarina Coquand" ]
        , paradigms = [ Paradigm.functional ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
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
            }
          , { type = ExampleType.vector
            , content =
                ''
                module hello-world-dep where

                open import Data.Nat using (ℕ; zero; suc)

                data Vec (A : Set) : ℕ → Set where
                  []  : Vec A zero
                  _∷_ : ∀ {n} (x : A) (xs : Vec A n) → Vec A (suc n)

                infixr 5 _∷_
                ''
            }
          ]
        , description =
            ''

            A dependently typed functional programming language and proof
            assistant. Agda is used for writing and verifying mathematical proofs
            and functional programs.

            ''
        , yearFirstPublished = 1999
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.dependent ]
        , resources =
          [ { title = "Agda Homepage"
            , type = ResourceType.homepage
            , href = "https://wiki.portal.chalmers.se/agda"
            }
          , { title = "Agda Documentation"
            , type = ResourceType.documentation
            , href = "https://agda.readthedocs.io"
            }
          , { title = "Agda GitHub Repository"
            , type = ResourceType.sourceRepository
            , href = "https://github.com/agda/agda"
            }
          , { title = "Programming Language Foundations in Agda"
            , type = ResourceType.book
            , href = "https://plfa.github.io"
            }
          ]
        }
      , { name = "Ari"
        , originalAuthors = [ "Kira Bruneau" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A type-centered purely functional programming language designed to
            type binary files. Ari focuses on providing strong type guarantees
            for working with binary data structures.

            ''
        , yearFirstPublished = 2022
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.sourceRepository
            , title = "Ari GitLab Repository"
            , href = "https://gitlab.com/ari-lang/ari"
            }
          ]
        }
      , { name = "Arturo"
        , originalAuthors = [ "Yanis Zafirópulos" ]
        , paradigms = [ Paradigm.functional ]
        , examples =
          [ { type = ExampleType.factorial
            , content =
                ''
                factorial: function [n][
                    (n > 1)? -> n * factorial n-1
                             -> 1
                ]
                0..10 | map => factorial
                      | select.first => [& > 123]
                      | print
                ''
            }
          ]
        , description =
            ''

            A programming language focused on simplicity and expressiveness, with
            built-in support for desktop, web, and shell scripting applications.

            ''
        , yearFirstPublished = 2019
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "Arturo Homepage"
            , type = ResourceType.homepage
            , href = "https://arturo-lang.io"
            }
          , { title = "Arturo Documentation"
            , type = ResourceType.documentation
            , href = "https://arturo-lang.io/documentation"
            }
          , { title = "Arturo Playground"
            , type = ResourceType.playground
            , href = "https://arturo-lang.io/playground"
            }
          ]
        }
      , { name = "Assembly"
        , originalAuthors = [ "Kathleen Booth", "Andrew Donald Booth" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            Generally a notation for machine language, assigning readable names
            (e.g. "mov") to machine instructors. Modern assembly languages often
            feature programmer conveniences such as labels and macros.

            ''
        , yearFirstPublished = 1947
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.weak ]
        , resources = [] : List Resource
        }
      , { name = "B"
        , originalAuthors = [ "Ken Thompson", "Dennis Ritchie" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A simplification of BCPL; predecessor to C. Like BCPL, B featured only
            one data type, the machine word (length of a register). The lack of
            support for ASCII characters was one motivating force behind the
            development of C.

            ''
        , yearFirstPublished = 1969
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.weak ]
        , resources = [] : List Resource
        }
      , { name = "BASIC"
        , originalAuthors = [ "John G. Kemeny", "Thomas E. Kurtz" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            Beginner's All-purpose Symbolic Instruction Code (BASIC) was designed
            to be easy to learn and use. It became widely popular in the home
            computer era of the 1980s.

            ''
        , yearFirstPublished = 1964
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources = [] : List Resource
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
        , paradigms = [ Paradigm.imperative ]
        , yearFirstPublished = 1967
        , examples = [] : List Example
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.weak ]
        , resources =
          [ { title = "BCPL Reference Manual"
            , type = ResourceType.languageReference
            , href = "https://www.cl.cam.ac.uk/~mr10/bcplman.pdf"
            }
          ]
        }
      , { name = "Ballerina"
        , originalAuthors = [ "WSO2" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                import ballerina/io;

                public function main() {
                    io:println("Hello World!");
                }
                ''
            }
          ]
        , description =
            ''

            An open-source, general-purpose programming language for cloud-native
            development. Ballerina is designed to simplify writing distributed
            programs.

            ''
        , yearFirstPublished = 2017
        , compilationTargets = [ CompilationTarget.jvmBytecode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Ballerina Homepage"
            , type = ResourceType.homepage
            , href = "https://ballerina.io"
            }
          , { title = "Ballerina Documentation"
            , type = ResourceType.documentation
            , href = "https://ballerina.io/learn"
            }
          ]
        }
      , { name = "Bash"
        , originalAuthors = [ "Brian Fox" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                echo "Hello, World!"
                ''
            }
          ]
        , description =
            ''

            The Bourne Again SHell (Bash) is a Unix shell and command language
            written as a free software replacement for the Bourne shell. It is widely
            available on various operating systems and is the default shell on most
            Linux distributions.

            ''
        , yearFirstPublished = 1989
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "Bash Homepage"
            , type = ResourceType.homepage
            , href = "https://www.gnu.org/software/bash"
            }
          , { title = "Bash Manual"
            , type = ResourceType.documentation
            , href = "https://www.gnu.org/software/bash/manual/bash.html"
            }
          ]
        }
      , { name = "Befunge"
        , originalAuthors = [ "Chris Pressey" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                >              v
                v  ,,,,,"Hello"<
                >48*,          v
                v,,,,,,"World!"<
                >25*,@
                ''
            }
          ]
        , description =
            ''

            An esoteric programming language where the program is laid out on a
            two-dimensional grid. The instruction pointer moves through this grid
            in cardinal directions, making programs that look like abstract art.
            Befunge was designed to be as difficult to compile as possible.

            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Befunge on Esolangs Wiki"
            , href = "https://esolangs.org/wiki/Befunge"
            }
          ]
        }
      , { name = "Brainfuck"
        , originalAuthors = [ "Urban Müller" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
                ''
            }
          ]
        , description =
            ''

            An esoteric programming language notable for its extreme minimalism.
            Brainfuck has only eight simple commands and operates on an array of
            memory cells. Despite its simplicity, it is Turing-complete.

            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.weak ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Brainfuck"
            , href = "https://en.wikipedia.org/wiki/Brainfuck"
            }
          ]
        }
      , { name = "C"
        , paradigms = [ Paradigm.imperative ]
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
          [ { type = ExampleType.helloWorld
            , content =
                ''
                #include <stdio.h>

                void main() {
                  printf("Hello, world!\n");
                }
                ''
            }
          ]
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.weak ]
        , resources =
          [ { title = "C Programming Language Standard"
            , type = ResourceType.languageSpecification
            , href = "https://www.iso.org/standard/82075.html"
            }
          , { title = "Learn C"
            , type = ResourceType.tutorial
            , href = "https://www.learn-c.org"
            }
          , { title = "Git"
            , type = ResourceType.notableApplication
            , href = "https://github.com/git/git"
            }
          , { title = "Linux Kernel"
            , type = ResourceType.notableApplication
            , href = "https://github.com/torvalds/linux"
            }
          ]
        }
      , { name = "C#"
        , originalAuthors =
          [ "Anders Hejlsberg", "Scott Wiltamuth", "Peter Golde" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A modern, object-oriented language developed by Microsoft as part of
            the .NET platform. While initially similar to Java, C# has evolved to
            incorporate a large variety of programming language features.

            C# is used for:
            - Desktop applications (Windows applications using WPF, WinForms, or WinUI)
            - Web applications and services (ASP.NET Core, Blazor)
            - Cloud-based applications and microservices (Azure services)
            - Game development (Unity game engine)
            - Mobile applications (Xamarin, .NET MAUI)

            ''
        , yearFirstPublished = 2000
        , compilationTargets = [ CompilationTarget.cil ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "C# Documentation"
            , type = ResourceType.documentation
            , href = "https://docs.microsoft.com/en-us/dotnet/csharp"
            }
          , { title = "Learn C#"
            , type = ResourceType.tutorial
            , href = "https://dotnet.microsoft.com/learn/csharp"
            }
          , { title = "ShareX"
            , type = ResourceType.notableApplication
            , href = "https://github.com/ShareX/ShareX"
            }
          , { title = "Jellyfin"
            , type = ResourceType.notableApplication
            , href = "https://github.com/jellyfin/jellyfin"
            }
          ]
        }
      , { name = "C++"
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , originalAuthors = [ "Bjarne Stroustroup" ]
        , yearFirstPublished = 1985
        , description =
            ''

            An popular and widely-used extension of C. While the first extension
            was first-class object-orientation (the code `c++` in C means "add
            one to c"), C++ now offers a large number and variety of extensions,
            including closures, templates, and exceptions.

            ''
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                #include <iostream>

                int main() {
                    std::cout << "Hello, World!" << std::endl;
                    return 0;
                }
                ''
            }
          ]
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "C++ Standard"
            , type = ResourceType.languageSpecification
            , href = "https://isocpp.org"
            }
          , { title = "CPP Reference"
            , type = ResourceType.languageReference
            , href = "https://en.cppreference.com"
            }
          , { title = "Blender"
            , type = ResourceType.notableApplication
            , href = "https://github.com/blender/blender"
            }
          , { title = "Godot Engine"
            , type = ResourceType.notableApplication
            , href = "https://github.com/godotengine/godot"
            }
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
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            COmmon Business-Oriented Language, an early programming language
            designed for business use. While rare, some COBOL code is still in use
            today.

            ''
        , yearFirstPublished = 1959
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "COBOL Documentation"
            , type = ResourceType.documentation
            , href = "https://www.ibm.com/docs/en/cobol-zos"
            }
          ]
        }
      , { name = "Cairo"
        , originalAuthors = [ "StarkWare" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            Cairo is a Rust-inspired programming language. It is used as the
            smart contract language for Starknet.

            ''
        , yearFirstPublished = 2020
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Cairo Homepage"
            , type = ResourceType.homepage
            , href = "https://www.cairo-lang.org"
            }
          , { title = "The Cairo Book"
            , type = ResourceType.book
            , href = "https://book.cairo-lang.org"
            }
          , { title = "Cairo Playground (online editor)"
            , type = ResourceType.playground
            , href = "https://www.cairo-lang.org/cairovm"
            }
          , { title = "Cairo GitHub Repository"
            , type = ResourceType.sourceRepository
            , href = "https://github.com/starkware-libs/cairo-lang"
            }
          ]
        }
      , { name = "Ceylon"
        , originalAuthors = [ "Gavin King" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            Eclipse Ceylon is a defunct programming language designed for writing
            large programs in teams originally developed by Red Hat. Ceylon
            emphasized readability, modularity, and type safety.

            ''
        , yearFirstPublished = 2011
        , compilationTargets =
          [ CompilationTarget.jvmBytecode
          , CompilationTarget.language { name = "JavaScript" }
          ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Ceylon Homepage"
            , type = ResourceType.homepage
            , href = "https://ceylon-lang.org"
            }
          ]
        }
      , { name = "Chez Scheme"
        , originalAuthors = [ "R. Kent Dybvig" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A high-performance superset of R6RS Scheme known for its speed and reliability. Chez Scheme was originally developed at Cadence Research Systems and was open sourced after Cadence was acquired by Cisco Systems. Chez Scheme ships with a second implementation, named Petite Chez Scheme, which uses a threaded interpreter as opposed to Chez Scheme's incremental native compilation.

            ''
        , yearFirstPublished = 1985
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Chez Scheme"
            , type = ResourceType.homepage
            , href = "https://cisco.github.io/ChezScheme"
            }
          , { title = "User's Guide"
            , type = ResourceType.documentation
            , href = "https://cisco.github.io/ChezScheme/csug9.5/csug.html"
            }
          ]
        }
      , { name = "Chicken Scheme"
        , originalAuthors = [ "Felix Winkelmann" ]
        , paradigms = [ Paradigm.functional ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                (print "Hello, World!")
                ''
            }
          ]
        , description =
            ''

            A Scheme that supports R5RS and R7RS and compiles to C. Known for its extensive library ecosystem and ease of deployment. CHICKEN Scheme aims to be free, simple, portable, extensible, well documented, and actively supported.

            ''
        , yearFirstPublished = 2000
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "CHICKEN Scheme"
            , type = ResourceType.homepage
            , href = "https://www.call-cc.org"
            }
          , { title = "Documentation"
            , type = ResourceType.documentation
            , href = "https://wiki.call-cc.org"
            }
          ]
        }
      , { name = "Clojure"
        , originalAuthors = [ "Rich Hickey" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A modern dialect of Lisp that runs on the Java Virtual Machine,
            emphasizing functional programming and immutability.

            ''
        , yearFirstPublished = 2007
        , compilationTargets =
          [ CompilationTarget.jvmBytecode
          , CompilationTarget.language { name = "JavaScript" }
          ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Clojure Homepage"
            , type = ResourceType.homepage
            , href = "https://clojure.org"
            }
          , { title = "Clojure Documentation"
            , type = ResourceType.documentation
            , href = "https://clojure.org/reference/documentation"
            }
          ]
        }
      , { name = "CoffeeScript"
        , originalAuthors = [ "Jeremy Ashkenas" ]
        , paradigms = [ Paradigm.functional, Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                console.log 'Hello, World!'
                ''
            }
          ]
        , description =
            ''

            A language that compiles to JavaScript, aiming to make code more
            readable and to provide syntactic sugar for common JavaScript idioms.
            Its syntax is inspired by Ruby, Python, and Haskell.

            ''
        , yearFirstPublished = 2009
        , compilationTargets =
          [ CompilationTarget.language { name = "JavaScript" } ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "CoffeeScript Homepage"
            , type = ResourceType.homepage
            , href = "https://coffeescript.org"
            }
          , { title = "CoffeeScript Documentation"
            , type = ResourceType.documentation
            , href = "https://coffeescript.org/#documentation"
            }
          ]
        }
      , { name = "ColdFusion Markup Langauge (CFML)"
        , originalAuthors = [ "J. J. Allaire", "Jeremy Allaire" ]
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , examples = [] : List Example
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
          [ CompilationTarget.jvmBytecode, CompilationTarget.cil ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "ColdFusion Homepage"
            , type = ResourceType.homepage
            , href = "https://www.adobe.com/products/coldfusion-family.html"
            }
          , { title = "ColdFusion Developers User Guide"
            , type = ResourceType.documentation
            , href = "https://helpx.adobe.com/coldfusion/cf-dev-user-guide.html"
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
          [ Paradigm.functional, Paradigm.imperative, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A standardized dialect of Lisp that unified several existing Lisp
            implementations. It is a multi-paradigm language featuring a macro
            system, dynamic typing, and support for multiple programming
            Paradigm.

            ''
        , yearFirstPublished = 1984
        , compilationTargets =
          [ CompilationTarget.machineCode, CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Common Lisp HyperSpec"
            , type = ResourceType.languageSpecification
            , href = "http://www.lispworks.com/documentation/HyperSpec/Front"
            }
          , { title = "Common Lisp Wiki"
            , type = ResourceType.wikipediaEntry
            , href = "https://www.cliki.net"
            }
          ]
        }
      , { name = "Crystal"
        , originalAuthors =
          [ "Ary Borenszweig", "Juan Wajnerman", "Brian Cardiff" ]
        , paradigms = [ Paradigm.objectOriented ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                puts "Hello, World!"
                ''
            }
          ]
        , description =
            ''

            A programming language with Ruby-like syntax that compiles to native
            code. It aims to provide the productivity of Ruby with the performance
            and type safety of a compiled language.

            ''
        , yearFirstPublished = 2014
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Crystal Homepage"
            , type = ResourceType.homepage
            , href = "https://crystal-lang.org"
            }
          , { title = "Crystal Documentation"
            , type = ResourceType.documentation
            , href = "https://crystal-lang.org/reference"
            }
          ]
        }
      , { name = "Curry"
        , originalAuthors = [ "Michael Hanus" ]
        , paradigms = [ Paradigm.functional, Paradigm.logic ]
        , examples = [] : List Example
        , description =
            ''

            A declarative programming language that combines functional
            programming with logic programming features. Named after Haskell
            B. Curry (also the namesake of the programming language Haskell), it
            integrates most of the important features of functional languages like
            Haskell and logic languages like Prolog.

            ''
        , yearFirstPublished = 1999
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Curry Homepage"
            , type = ResourceType.homepage
            , href = "http://www.curry-lang.org"
            }
          , { title = "Curry Documentation"
            , type = ResourceType.documentation
            , href = "http://www.curry-lang.org/documentation"
            }
          ]
        }
      , { name = "D"
        , originalAuthors = [ "Walter Bright", "Andrei Alexandrescu" ]
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , examples =
          [ { type = ExampleType.quicksort
            , content =
                ''
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
            }
          ]
        , description =
            ''

            D is a general-purpose programming language which is intended to be
            similar to, but an improvement upon, C and C++. Unlike those
            languages, D supports automatic memory management via garbage
            collection as well as manual memory management.

            ''
        , yearFirstPublished = 2001
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "D Language Homepage"
            , type = ResourceType.homepage
            , href = "https://dlang.org"
            }
          , { title = "D Language Documentation"
            , type = ResourceType.documentation
            , href = "https://dlang.org/documentation.html"
            }
          ]
        }
      , { name = "Dart"
        , originalAuthors = [ "Lars Bak", "Kasper Lund" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A client-optimized programming language developed by Google for
            building web, mobile, and desktop applications. Originally designed as
            a replacement for JavaScript, it now focuses on being a
            general-purpose language with strong tooling support. Dart is the
            language used by the Flutter application framework.

            ''
        , yearFirstPublished = 2011
        , compilationTargets =
          [ CompilationTarget.language { name = "JavaScript" } ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Dart Homepage"
            , type = ResourceType.homepage
            , href = "https://dart.dev"
            }
          , { title = "Dart Documentation"
            , type = ResourceType.documentation
            , href = "https://dart.dev/guides"
            }
          , { title = "Dartpad (Online Editor)"
            , type = ResourceType.playground
            , href = "https://dartpad.dev"
            }
          , { title = "Flutter"
            , type = ResourceType.popularLibrary
            , href = "https://flutter.dev"
            }
          ]
        }
      , { name = "Datalog"
        , originalAuthors = [ "Hervé Gallaire", "Jack Minker" ]
        , paradigms = [ Paradigm.logic, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A declarative logic programming language that is a subset of Prolog,
            but typically uses a bottom-up rather than top-down evaluation style.
            It is often used as a query language for deductive databases such as
            Datomic.

            ''
        , yearFirstPublished = 1977
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.static ]
        , resources =
          [ { title = "Datalog Draft Specification"
            , type = ResourceType.languageSpecification
            , href = "https://datalog-specs.info"
            }
          , { title = "Datomic Database"
            , type = ResourceType.implementation
            , href = "https://datomic.com"
            }
          ]
        }
      , { name = "Delphi"
        , originalAuthors =
          [ "Borland Software Corporation"
          , "CodeGear"
          , "Embarcadero Technologies"
          , "Anders Hejlsberg"
          ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A high-level, compiled, strongly typed language that evolved from
            Pascal. Delphi is known for its rapid application development (RAD)
            capabilities, particularly for Windows applications.

            ''
        , yearFirstPublished = 1995
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Delphi Homepage"
            , type = ResourceType.homepage
            , href = "https://www.embarcadero.com/products/delphi"
            }
          , { title = "Delphi Documentation"
            , type = ResourceType.documentation
            , href = "https://docwiki.embarcadero.com/RADStudio/en/Main_Page"
            }
          ]
        }
      , { name = "Dhall"
        , originalAuthors = [ "Gabriel Gonzalez" ]
        , paradigms = [ Paradigm.functional, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A programmable configuration language that is not Turing
            complete. Dhall aims to be a standardized configuration language that
            is guaranteed to terminate and is more expressive than JSON or
            YAML. Unlike those formats, Dhall can specify types for data which
            ensures that data being loaded by an application has a correct format.

            ''
        , yearFirstPublished = 2017
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Dhall Homepage"
            , type = ResourceType.homepage
            , href = "https://dhall-lang.org"
            }
          , { title = "Dhall Documentation"
            , type = ResourceType.documentation
            , href = "https://docs.dhall-lang.org"
            }
          , { title = "Dhall GitHub"
            , type = ResourceType.sourceRepository
            , href = "https://github.com/dhall-lang/dhall-lang"
            }
          ]
        }
      , { name = "Dusa"
        , originalAuthors = [ "Rob Simmons" ]
        , paradigms = [ Paradigm.logic, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A logic programming language that explores bottom-up logical
            inference with support for open and closed world reasoning.

            ''
        , yearFirstPublished = 2023
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.documentation
            , title = "Dusa Documentation"
            , href = "https://dusa.rocks/docs"
            }
          ]
        }
      , { name = "Eiffel"
        , originalAuthors = [ "Bertrand Meyer" ]
        , paradigms = [ Paradigm.declarative, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            An object-oriented programming language designed to promote software
            quality through the use of Design by Contract (DbC). Eiffel emphasizes
            readability, reusability, and reliability.

            ''
        , yearFirstPublished = 1986
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Eiffel Homepage"
            , type = ResourceType.homepage
            , href = "https://www.eiffel.org"
            }
          , { title = "Eiffel Documentation"
            , type = ResourceType.documentation
            , href = "https://docs.eiffel.org"
            }
          ]
        }
      , { name = "Elixir"
        , originalAuthors = [ "José Valim" ]
        , paradigms = [ Paradigm.functional, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A functional programming language with Ruby-like syntax that runs on
            the BEAM (the Erlang virtual machine). Elixir is commonly used with
            the web server library Phoenix. Elixir takes heavy inspiration from
            Erlang and Elixir applications frequently leverage Erlang libraries.

            ''
        , yearFirstPublished = 2011
        , compilationTargets = [ CompilationTarget.beamBytecode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Elixir Homepage"
            , type = ResourceType.homepage
            , href = "https://elixir-lang.org"
            }
          , { title = "Elixir Documentation"
            , type = ResourceType.documentation
            , href = "https://hexdocs.pm/elixir"
            }
          , { title = "Phoenix Framework"
            , type = ResourceType.popularLibrary
            , href = "https://www.phoenixframework.org"
            }
          ]
        }
      , { name = "Elm"
        , originalAuthors = [ "Evan Czaplicki" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A functional programming language that compiles to JavaScript,
            specifically designed for building web browser-based user
            interfaces. Elm emphasizes simplicity, ease of use (including friendly
            error messages), and no runtime exceptions.

            ''
        , yearFirstPublished = 2012
        , compilationTargets =
          [ CompilationTarget.language { name = "JavaScript" } ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Elm Homepage"
            , type = ResourceType.homepage
            , href = "https://elm-lang.org"
            }
          , { title = "Elm Guide"
            , type = ResourceType.tutorial
            , href = "https://guide.elm-lang.org"
            }
          , { title = "Elm Package Documentation"
            , type = ResourceType.documentation
            , href = "https://package.elm-lang.org"
            }
          ]
        }
      , { name = "Emacs Lisp"
        , originalAuthors = [ "Richard Stallman" ]
        , paradigms = [ Paradigm.functional, Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                (message "Hello, World!")
                ''
            }
          , { type = ExampleType.fibonacci
            , content =
                ''
                (defun fib (n)
                  (if (< n 2)
                      n
                    (+ (fib (- n 1)) (fib (- n 2)))))
                ''
            }
          ]
        , description =
            ''

            A dialect of the Lisp programming language used as a scripting language by the Emacs text editor. Emacs Lisp provides text manipulation capabilities and is used to extend and customize Emacs.

            ''
        , yearFirstPublished = 1985
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "Emacs Lisp Introduction"
            , type = ResourceType.tutorial
            , href =
                "https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html"
            }
          , { title = "Emacs Lisp Reference Manual"
            , type = ResourceType.languageReference
            , href =
                "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html"
            }
          , { title = "GNU Emacs"
            , type = ResourceType.notableApplication
            , href = "https://github.com/emacs-mirror/emacs"
            }
          ]
        }
      , { name = "Erlang"
        , originalAuthors =
          [ "Joe Armstrong", "Robert Virding", "Mike Williams" ]
        , paradigms = [ Paradigm.functional, Paradigm.declarative ]
        , examples = [] : List Example
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
        , compilationTargets = [ CompilationTarget.beamBytecode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Erlang Homepage"
            , type = ResourceType.homepage
            , href = "https://www.erlang.org"
            }
          , { title = "Erlang Documentation"
            , type = ResourceType.documentation
            , href = "https://www.erlang.org/docs"
            }
          , { title = "RabbitMQ"
            , type = ResourceType.notableApplication
            , href = "https://github.com/rabbitmq/rabbitmq-server"
            }
          , { title = "CouchDB"
            , type = ResourceType.notableApplication
            , href = "https://github.com/apache/couchdb"
            }
          ]
        }
      , { name = "Euphoria"
        , originalAuthors = [ "Robert Craig" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                procedure Hello(sequence place)
                  puts(1, "Hello, " & place & "!\n")
                end procedure

                Hello("World")
                ''
            }
          ]
        , description =
            ''

            The Euphoria programming language strives to be simple, flexible, and
            easy-to-learn. It was designed to be easy to read and write, and
            prefers English keywords over symbols in its syntax.

            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "Open Euphoria Homepage"
            , type = ResourceType.homepage
            , href = "http://openeuphoria.org"
            }
          , { title = "Open Euphoria Manual"
            , type = ResourceType.documentation
            , href = "https://openeuphoria.org/docs"
            }
          , { title = "Rapide Euphoria Homepage"
            , type = ResourceType.homepage
            , href = "https://www.rapideuphoria.com"
            }
          , { title = "Rapide Euphoria Documentation"
            , type = ResourceType.documentation
            , href = "https://www.rapideuphoria.com/docs.htm"
            }
          ]
        }
      , { name = "F#"
        , originalAuthors = [ "Don Syme" ]
        , paradigms = [ Paradigm.functional, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A ML-like, functional-first programming language for the .NET
            ecosystem.

            ''
        , yearFirstPublished = 2005
        , compilationTargets = [ CompilationTarget.cil ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { title = "F# Homepage"
            , type = ResourceType.homepage
            , href = "https://fsharp.org"
            }
          , { title = "F# Documentation"
            , type = ResourceType.documentation
            , href = "https://docs.microsoft.com/en-us/dotnet/fsharp"
            }
          ]
        }
      , { name = "F*"
        , originalAuthors =
          [ "Antoine Delignat-Lavaud", "Microsoft Research", "Inria" ]
        , paradigms = [ Paradigm.functional, Paradigm.objectOriented ]
        , examples =
          [ { type = ExampleType.vector
            , content =
                ''
                type vec (a:Type) : nat -> Type =
                  | Nil : vec a 0
                  | Cons : #n:nat -> hd:a -> tl:vec a n -> vec a (n + 1)
                ''
            }
          ]
        , description =
            ''

            F* is a general-purpose, proof-oriented programming language featuring
            dependent types that compiles to C, F#, OCaml, or WASM.

            ''
        , yearFirstPublished = 2011
        , compilationTargets =
          [ CompilationTarget.language { name = "C" }
          , CompilationTarget.language { name = "F#" }
          , CompilationTarget.language { name = "OCaml" }
          , CompilationTarget.webassembly
          ]
        , typing = [ TypingSystem.dependent, TypingSystem.static ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "F* Homepage"
            , href = "https://fstar-lang.org/"
            }
          , { type = ResourceType.tutorial
            , title = "Proof-Oriented Programming in F* (PDF)"
            , href =
                "https://fstar-lang.org/tutorial/proof-oriented-programming-in-fstar.pdf"
            }
          ]
        }
      , { name = "Factor"
        , originalAuthors = [ "Slava Pestov" ]
        , paradigms =
          [ Paradigm.functional, Paradigm.declarative, Paradigm.stack ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                "Hello, World!" print
                ''
            }
          ]
        , description =
            ''

            A stack-oriented programming language with high-level features like
            dynamic typing, extensible syntax, macros, and garbage
            collection. Factor emphasizes interactive development and
            concatenative programming.

            ''
        , yearFirstPublished = 2003
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Factor Homepage"
            , type = ResourceType.homepage
            , href = "https://factorcode.org"
            }
          , { title = "Factor Documentation"
            , type = ResourceType.documentation
            , href = "https://docs.factorcode.org"
            }
          , { title = "Factor GitHub Repository"
            , type = ResourceType.sourceRepository
            , href = "https://github.com/factor/factor"
            }
          ]
        }
      , { name = "False"
        , originalAuthors = [ "Wouter van Oortmerssen" ]
        , paradigms = [ Paradigm.stack, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            An esoteric stack-based programming language designed to be highly
            compact. False influenced the design of Brainfuck and other
            minimalist languages.

            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.weak ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "False Language Homepage"
            , href = "https://strlen.com/false-language/"
            }
          ]
        }
      , { name = "Flix"
        , originalAuthors = [ "Magnus Madsen" ]
        , paradigms = [ Paradigm.functional, Paradigm.logic ]
        , examples = [] : List Example
        , description =
            ''

            A functional and logic programming language inspired by Scala, OCaml,
            and Datalog. Flix features a unique type and effect system and
            supports both functional programming and Datalog-style logic
            programming.

            ''
        , yearFirstPublished = 2016
        , compilationTargets = [ CompilationTarget.jvmBytecode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Flix Homepage"
            , href = "https://flix.dev"
            }
          , { type = ResourceType.documentation
            , title = "Flix Documentation"
            , href = "https://flix.dev/documentation/"
            }
          ]
        }
      , { name = "Forth"
        , originalAuthors = [ "Charles H. Moore" ]
        , paradigms = [ Paradigm.imperative, Paradigm.stack ]
        , examples = [] : List Example
        , description =
            ''

            A stack-based programming language emphasizing simplicity and
            extensibility.

            ''
        , yearFirstPublished = 1970
        , compilationTargets =
          [ CompilationTarget.interpreted, CompilationTarget.machineCode ]
        , typing = [ TypingSystem.weak ]
        , resources =
          [ { title = "Forth Interest Group"
            , type = ResourceType.homepage
            , href = "http://www.forth.org"
            }
          ]
        }
      , { name = "Fortran"
        , originalAuthors = [ "John Backus" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            The first high-level programming language, designed for scientific
            computing. While no longer popular, Fortran is still occasionally used
            in scientific computing.

            ''
        , yearFirstPublished = 1957
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Fortran Homepage"
            , type = ResourceType.homepage
            , href = "https://fortran-lang.org"
            }
          , { title = "Fortran Documentation"
            , type = ResourceType.documentation
            , href = "https://fortran-lang.org/learn"
            }
          ]
        }
      , { name = "Gleam"
        , originalAuthors = [ "Louis Pilfold" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A statically typed functional programming language. Gleam's compiler
            is implemented in Rust, and the language has many syntactic and
            semantic similarities to Rust. Gleam can compile to BEAM (Erlang
            virtual machine) bytecode or JavaScript, and can call functions
            available in those environments (FFI).

            ''
        , yearFirstPublished = 2016
        , compilationTargets = [ CompilationTarget.beamBytecode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { title = "Gleam Homepage"
            , type = ResourceType.homepage
            , href = "https://gleam.run"
            }
          , { title = "Gleam Documentation"
            , type = ResourceType.documentation
            , href = "https://gleam.run/documentation"
            }
          , { title = "Gleam Discord"
            , type = ResourceType.tutorial
            , href = "https://discord.gg/Fm8Pwmy"
            }
          ]
        }
      , { name = "Go"
        , originalAuthors = [ "Robert Griesemer", "Rob Pike", "Ken Thompson" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                package main

                import "fmt"

                func main() {
                	fmt.Println("Hello, 世界")
                }
                ''
            }
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
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Go Homepage"
            , type = ResourceType.homepage
            , href = "https://golang.org"
            }
          , { title = "Go Documentation"
            , type = ResourceType.documentation
            , href = "https://golang.org/doc"
            }
          , { title = "Docker"
            , type = ResourceType.notableApplication
            , href = "https://github.com/docker/docker"
            }
          , { title = "Kubernetes"
            , type = ResourceType.notableApplication
            , href = "https://github.com/kubernetes/kubernetes"
            }
          ]
        }
      , { name = "Grain"
        , originalAuthors = [ "Oscar Spencer", "Blaine Bublitz" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A strongly-typed functional programming language that compiles to
            WebAssembly. Grain has syntax and semantics similar to Rust, but
            with some notable changes (no borrow checking or lifetimes, row
            types). Semantically, Grain is close to PureScript.

            ''
        , yearFirstPublished = 2017
        , compilationTargets = [ CompilationTarget.webassembly ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Grain Homepage"
            , href = "https://grain-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Grain Documentation"
            , href = "https://grain-lang.org/docs/"
            }
          ]
        }
      , { name = "Groovy"
        , originalAuthors = [ "James Strachan" ]
        , paradigms =
          [ Paradigm.objectOriented, Paradigm.functional, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            An object-oriented programming language for the Java platform. Most
            valid Java files are also valid Groovy files, allowing Java developers
            to easily pick up the language. Groovy features more concise syntax
            than Java for certain operations, as well as dynamic typing, operator
            overloading, and more.

            ''
        , yearFirstPublished = 2003
        , compilationTargets = [ CompilationTarget.jvmBytecode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Groovy Homepage"
            , type = ResourceType.homepage
            , href = "https://groovy-lang.org"
            }
          , { title = "Groovy Documentation"
            , type = ResourceType.documentation
            , href = "https://groovy-lang.org/documentation.html"
            }
          ]
        }
      , { name = "Guile"
        , originalAuthors =
          [ "Aubrey Jaffer", "Tom Lord", "Miles Bader", "Jim Blandy" ]
        , paradigms = [ Paradigm.functional, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            GNU Ubiquitous Intelligent Language for Extensions (Guile) is an
            implementation of the Scheme programming language, designed to be
            embedded in other applications to provide scripting and extension
            capabilities.

            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "Guile Homepage"
            , type = ResourceType.homepage
            , href = "https://www.gnu.org/software/guile"
            }
          , { title = "Guile Documentation"
            , type = ResourceType.documentation
            , href = "https://www.gnu.org/software/guile/docs"
            }
          ]
        }
      , { name = "Hack"
        , originalAuthors =
          [ "Julien Verlaguet", "Alok Menghrajani", "Drew Paroski" ]
        , paradigms =
          [ Paradigm.imperative, Paradigm.functional, Paradigm.objectOriented ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                use namespace HH\Lib\IO;

                <<__EntryPoint>>
                async function main(): Awaitable<void> {
                  await IO\request_output()->writeAllAsync("Hello World!\n");
                }
                ''
            }
          ]
        , description =
            ''

            Hack was developed at Facebook as a derivative of PHP. Much PHP code
            is compatible with Hack. Hack runs on the HipHop Virtual Machine
            (HHVM), also developed at Facebook.

            ''
        , yearFirstPublished = 2014
        , typing = [ TypingSystem.gradual ]
        , compilationTargets = [ CompilationTarget.interpreted ]
        , resources =
          [ { title = "Hack Website"
            , type = ResourceType.homepage
            , href = "https://hacklang.org"
            }
          ]
        }
      , { name = "Hare"
        , originalAuthors = [ "Drew DeVault" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                use fmt;

                export fn main() void = {
                	fmt::println("Hello, World!")!;
                };
                ''
            }
          ]
        , description =
            ''

            Hare is a systems programming language designed to be simple, stable,
            and robust. It aims to provide a safer alternative to C while maintaining
            direct hardware access and manual memory management. The language features
            a minimal runtime and no hidden control flow.

            ''
        , yearFirstPublished = 2022
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { title = "Hare Homepage"
            , type = ResourceType.homepage
            , href = "https://harelang.org"
            }
          , { title = "Hare Tutorial"
            , type = ResourceType.tutorial
            , href = "https://harelang.org/tutorials/introduction/"
            }
          , { title = "Hare Documentation"
            , type = ResourceType.documentation
            , href = "https://docs.harelang.org"
            }
          , { title = "Hare Source Repository"
            , type = ResourceType.sourceRepository
            , href = "https://git.sr.ht/~sircmpwn/hare"
            }
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
        , paradigms = [ Paradigm.functional, Paradigm.declarative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                main :: IO ()
                main = putStrLn "Hello, World!"
                ''
            }
          ]
        , description =
            ''

            A purely functional programming language with strong static typing and
            lazy evaluation. Haskell is commonly the foundation for functional
            programming language research.

            ''
        , yearFirstPublished = 1990
        , compilationTargets =
          [ CompilationTarget.machineCode
          , CompilationTarget.language { name = "JavaScript" }
          , CompilationTarget.webassembly
          ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { title = "Haskell Homepage"
            , type = ResourceType.homepage
            , href = "https://www.haskell.org"
            }
          , { title = "Haskell Documentation"
            , type = ResourceType.documentation
            , href = "https://www.haskell.org/documentation"
            }
          , { title = "Pandoc"
            , type = ResourceType.notableApplication
            , href = "https://github.com/jgm/pandoc"
            }
          , { title = "xmonad"
            , type = ResourceType.notableApplication
            , href = "https://github.com/xmonad/xmonad"
            }
          ]
        }
      , { name = "INTERCAL"
        , originalAuthors = [ "Don Woods", "James M. Lyon" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            An esoteric programming language created as a parody of languages of
            the time. INTERCAL (Compiler Language With No Pronounceable Acronym)
            features intentionally complex syntax and unusual control structures.

            ''
        , yearFirstPublished = 1972
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.weak ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: INTERCAL"
            , href = "https://en.wikipedia.org/wiki/INTERCAL"
            }
          ]
        }
      , { name = "Idris"
        , originalAuthors = [ "Edwin Brady" ]
        , paradigms = [ Paradigm.functional ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                main : IO ()
                main = putStrLn "Hello, World!"
                ''
            }
          , { type = ExampleType.vector
            , content =
                ''
                data Vect : Nat -> Type -> Type where
                  Nil : Vect Z a
                  (::) : a -> Vect n a -> Vect (S n) a
                ''
            }
          ]
        , description =
            ''

            A general-purpose functional programming language with dependent
            types, which allows types to be predicated on values. Idris aims to
            provide a practical programming language with full dependent types.

            ''
        , yearFirstPublished = 2013
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.dependent ]
        , resources =
          [ { title = "Idris Homepage"
            , type = ResourceType.homepage
            , href = "https://www.idris-lang.org"
            }
          , { title = "Idris Documentation"
            , type = ResourceType.documentation
            , href = "https://idris2.readthedocs.io/en/latest"
            }
          , { title = "Idris GitHub Repository"
            , type = ResourceType.sourceRepository
            , href = "https://github.com/idris-lang/Idris2"
            }
          ]
        }
      , { name = "Io"
        , originalAuthors = [ "Steve Dekorte" ]
        , paradigms = [ Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A pure object-oriented programming language inspired by Smalltalk,
            Self, and Lisp. Everything in Io is a message that is passed to
            objects. It features a small core with highly dynamic and reflective
            capabilities.

            ''
        , yearFirstPublished = 2002
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Io Language"
            , type = ResourceType.homepage
            , href = "https://iolanguage.org"
            }
          , { title = "Io Guide"
            , type = ResourceType.tutorial
            , href = "https://iolanguage.org/guide/guide.html"
            }
          ]
        }
      , { name = "Ioke"
        , originalAuthors = [ "Ola Bini" ]
        , paradigms = [ Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A prototype-based, dynamic programming language inspired by Io,
            Smalltalk, Ruby, and Lisp. It runs on the Java Virtual Machine and
            emphasizes expressiveness and experimentation.

            ''
        , yearFirstPublished = 2008
        , compilationTargets = [ CompilationTarget.jvmBytecode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { title = "Ioke Language"
            , type = ResourceType.homepage
            , href = "https://ioke.org"
            }
          , { title = "Ioke Documentation"
            , type = ResourceType.documentation
            , href = "https://ioke.org/documentation.html"
            }
          ]
        }
      , { name = "J"
        , originalAuthors = [ "Kenneth E. Iverson", "Roger Hui" ]
        , paradigms =
          [ Paradigm.array, Paradigm.declarative, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A successor to APL focusing on array programming using only the ASCII
            character set.

            ''
        , yearFirstPublished = 1990
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { title = "J Homepage"
            , type = ResourceType.homepage
            , href = "https://www.jsoftware.com"
            }
          , { title = "J Documentation"
            , type = ResourceType.documentation
            , href = "https://code.jsoftware.com/wiki/Main_Page"
            }
          ]
        }
      , { name = "Java"
        , originalAuthors = [ "James Gosling" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A class-based, object-oriented programming language designed to be
            'write once, run anywhere' via the Java Virtual Machine. Originally
            designed at Sun Microsystems, Java is currently owned by Oracle. Java
            is one of the most widely-used programming languages today.

            ''
        , yearFirstPublished = 1995
        , compilationTargets = [ CompilationTarget.jvmBytecode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.nominal ]
        , resources =
          [ { title = "Java Homepage"
            , type = ResourceType.homepage
            , href = "https://www.java.com"
            }
          , { title = "Java Documentation"
            , type = ResourceType.documentation
            , href = "https://docs.oracle.com/en/java"
            }
          , { title = "Elasticsearch"
            , type = ResourceType.notableApplication
            , href = "https://github.com/elastic/elasticsearch"
            }
          , { title = "Jenkins"
            , type = ResourceType.notableApplication
            , href = "https://github.com/jenkinsci/jenkins"
            }
          ]
        }
      , { name = "JavaScript"
        , originalAuthors = [ "Brendan Eich" ]
        , paradigms =
          [ Paradigm.imperative, Paradigm.functional, Paradigm.objectOriented ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                console.log("Hello, World!");
                ''
            }
          , { type = ExampleType.map
            , content =
                ''
                function addOne(x) {
                  return x + 1;
                }
                ''
            }
          , { type = ExampleType.map
            , content =
                ''
                const addOne = (x) => x + 1;
                ''
            }
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
        , compilationTargets =
          [ CompilationTarget.language { name = "JavaScript" } ]
        , typing = [ TypingSystem.dynamic, TypingSystem.weak ]
        , resources =
          [ { title = "MDN JavaScript Guide"
            , type = ResourceType.tutorial
            , href =
                "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide"
            }
          , { title = "ECMAScript Specification"
            , type = ResourceType.languageSpecification
            , href =
                "https://www.ecma-international.org/publications-and-standards/standards/ecma-262"
            }
          , { title = "Visual Studio Code"
            , type = ResourceType.notableApplication
            , href = "https://github.com/microsoft/vscode"
            }
          , { title = "Node.js"
            , type = ResourceType.implementation
            , href = "https://github.com/nodejs/node"
            }
          ]
        }
      , { name = "Julia"
        , originalAuthors =
          [ "Jeff Bezanson"
          , "Stefan Karpinski"
          , "Viral B. Shah"
          , "Alan Edelman"
          ]
        , paradigms = [ Paradigm.functional, Paradigm.imperative ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content = "println(\"Hello, World!\")"
                }
              ]
            : List Example
        , description =
            ''

            A high-performance, general-purpose programming language, with syntax
            that is familiar to users of scripting languages. Julia provides a
            sophisticated compiler, distributed parallel execution, numerical
            accuracy, and an extensive mathematical function library.

            ''
        , yearFirstPublished = 2012
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.dynamic
          , TypingSystem.inferred
          , TypingSystem.nominal
          , TypingSystem.strong
          ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Julia Homepage"
            , href = "https://julialang.org"
            }
          , { type = ResourceType.documentation
            , title = "Julia Documentation"
            , href = "https://docs.julialang.org"
            }
          , { type = ResourceType.sourceRepository
            , title = "Julia GitHub Repository"
            , href = "https://github.com/JuliaLang/julia"
            }
          ]
        }
      , { name = "K"
        , originalAuthors = [ "Arthur Whitney" ]
        , paradigms = [ Paradigm.array, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            An array programming language used primarily in financial
            applications.

            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Kx Systems"
            , href = "https://kx.com"
            }
          ]
        }
      , { name = "Koka"
        , originalAuthors = [ "Daan Leijen" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A strongly-typed functional programming language with a focus on
            effect systems and algebraic effects. Koka aims to provide a clear
            separation between pure and effectful computations.

            ''
        , yearFirstPublished = 2014
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Koka Homepage"
            , href = "https://koka-lang.github.io/koka"
            }
          , { type = ResourceType.documentation
            , title = "Koka Documentation"
            , href = "https://koka-lang.github.io/koka/doc/book.html"
            }
          , { type = ResourceType.sourceRepository
            , title = "Koka GitHub Repository"
            , href = "https://github.com/koka-lang/koka"
            }
          ]
        }
      , { name = "Kotlin"
        , originalAuthors = [ "JetBrains Team" ]
        , paradigms =
          [ Paradigm.objectOriented
          , Paradigm.functional
          , Paradigm.imperative
          , Paradigm.declarative
          ]
        , examples = [] : List Example
        , description =
            ''

            A modern programming language developed by IDE vendor JetBrains
            targeting the JVM, Android, and web browsers (via compilation to
            JavaScript). Kotlin is the default for new Android apps in the
            official Android Sutdio IDE (also made by JetBrains).

            ''
        , yearFirstPublished = 2011
        , compilationTargets =
          [ CompilationTarget.jvmBytecode
          , CompilationTarget.language { name = "JavaScript" }
          ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Kotlin Homepage"
            , href = "https://kotlinlang.org"
            }
          , { type = ResourceType.documentation
            , title = "Kotlin Documentation"
            , href = "https://kotlinlang.org/docs"
            }
          ]
        }
      , { name = "Lean"
        , originalAuthors = [ "Leonardo de Moura" ]
        , paradigms = [ Paradigm.functional ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                def main : IO Unit :=
                  IO.println "Hello, World!"
                ''
            }
          ]
        , description =
            ''

            A functional programming language and interactive theorem prover
            developed at Microsoft Research. Lean is designed for both
            mathematics formalization and general-purpose programming. It
            features dependent types which facilitates mathematical theorems to
            be expressed and proven. Lean 4, the latest version, includes
            significant improvements to performance and usability as a
            programming language while maintaining its capabilities as a proof
            assistant.

            ''
        , yearFirstPublished = 2013
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.dependent ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Lean Homepage"
            , href = "https://lean-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Lean 4 Documentation"
            , href = "https://lean-lang.org/lean4/doc"
            }
          , { type = ResourceType.book
            , title = "Theorem Proving in Lean 4"
            , href = "https://lean-lang.org/theorem_proving_in_lean4"
            }
          , { type = ResourceType.playground
            , title = "Lean Web Editor"
            , href = "https://live.lean-lang.org"
            }
          , { type = ResourceType.sourceRepository
            , title = "Lean 4 GitHub Repository"
            , href = "https://github.com/leanprover/lean4"
            }
          , { type = ResourceType.wikipediaEntry
            , title = "Lean Wikipedia Entry"
            , href = "https://en.wikipedia.org/wiki/Lean_(proof_assistant)"
            }
          ]
        }
      , { name = "Lisp"
        , originalAuthors = [ "John McCarthy", "Steve Russell" ]
        , paradigms = [ Paradigm.functional, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            The original LISt Processing language, designed for symbolic
            computation and known for its simple, yet powerful, syntax based on
            S-expressions. Lisp has influenced many other programming languages
            and has numerous dialects, including Common Lisp and Scheme.

            ''
        , yearFirstPublished = 1958
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Lisp (programming language)"
            , href = "https://en.wikipedia.org/wiki/Lisp_(programming_language)"
            }
          ]
        }
      , { name = "Lisp Flavored Erlang (LFE)"
        , originalAuthors = [ "Robert Virding" ]
        , paradigms = [ Paradigm.functional, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A Lisp syntax front-end to BEAM (the Erlang virtual machine). LFE combines the power of
            Lisp with the robustness and concurrency of the Erlang ecosystem.

            ''
        , yearFirstPublished = 2007
        , compilationTargets = [ CompilationTarget.beamBytecode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "LFE Homepage"
            , href = "https://lfe.io"
            }
          , { type = ResourceType.documentation
            , title = "LFE Documentation"
            , href = "https://docs.lfe.io"
            }
          , { type = ResourceType.sourceRepository
            , title = "LFE GitHub Repository"
            , href = "https://github.com/lfe/lfe"
            }
          ]
        }
      , { name = "Low*"
        , originalAuthors = [ "Microsoft Research", "Inria" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            Low* is a subset of the F* programming language, tailored for
            low-level programming. To this end, Low* does not have garbage
            collection or implicit heap allocation. Instead, Low* can guarantee
            memory safety through its (F*'s), type system.

            ''
        , yearFirstPublished = 2017
        , compilationTargets = [ CompilationTarget.language { name = "C" } ]
        , typing = [ TypingSystem.static ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "The Low* Subset of F*"
            , href = "https://fstarlang.github.io/lowstar/html/LowStar.html"
            }
          ]
        }
      , { name = "Lua"
        , originalAuthors =
          [ "Roberto Ierusalimschy"
          , "Waldemar Celes"
          , "Luiz Henrique de Figueiredo"
          ]
        , paradigms = [ Paradigm.imperative, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A lightweight, high-level scripting language designed primarily for
            embedded use in applications. Known for its efficiency, portability,
            and ease of integration.

            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Lua Homepage"
            , href = "https://www.lua.org"
            }
          , { type = ResourceType.documentation
            , title = "Lua Documentation"
            , href = "https://www.lua.org/docs.html"
            }
          ]
        }
      , { name = "MATLAB"
        , originalAuthors = [ "Cleve Moler" ]
        , paradigms = [ Paradigm.array, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            Originally "MATrix LABoratory", MATLAB is a widely-used,
            closed-source numerical computing environment and programming
            language.

            ''
        , yearFirstPublished = 1984
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "MATLAB Homepage"
            , href = "https://www.mathworks.com/products/matlab.html"
            }
          , { type = ResourceType.documentation
            , title = "MATLAB Documentation"
            , href = "https://www.mathworks.com/help/matlab"
            }
          ]
        }
      , { name = "Magik"
        , originalAuthors = [ "Arthur Chance" ]
        , paradigms = [ Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A dynamic object-oriented programming language originally developed by
            Smallworld for developing geographical information systems (GIS), now
            offered by GE Energy. Features multiple inheritance, dynamic typing,
            and garbage collection.

            ''
        , yearFirstPublished = 1990
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Magik Development Tools (MDT)"
            , href = "https://mdt.net"
            }
          , { type = ResourceType.popularLibrary
            , title = "Visual Studio Code Extension"
            , href =
                "https://marketplace.visualstudio.com/items?itemName=siamz.smallworld-magik"
            }
          ]
        }
      , { name = "Mercury"
        , originalAuthors = [ "Zoltan Somogyi", "Ralph Becket", "Peter Ross" ]
        , paradigms = [ Paradigm.functional, Paradigm.logic ]
        , examples = [] : List Example
        , description =
            ''

            A logic/functional programming language with a Prolog-like syntax,
            featuring advanced static analysis and error detection
            features. Mercury improves upon Prolog with a strong type system, mode
            system, and determinism analysis.

            ''
        , yearFirstPublished = 1995
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Mercury Homepage"
            , href = "https://mercurylang.org"
            }
          , { type = ResourceType.documentation
            , title = "Mercury Documentation"
            , href = "https://mercurylang.org/documentation/documentation.html"
            }
          ]
        }
      , { name = "Modula"
        , originalAuthors = [ "Niklaus Wirth" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A successor to Pascal emphasizing modularity, wherein groups of
            related declarations are grouped into modules.

            ''
        , yearFirstPublished = 1975
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources = [] : List Resource
        }
      , { name = "Modula-2"
        , originalAuthors = [ "Niklaus Wirth" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A successor to Modula with improved type safety and module
            system. Modula-2 added support for separate compilation, co-routines
            for concurrent programming, and a more sophisticated module system. It
            influenced later languages like Ada and Oberon.

            ''
        , yearFirstPublished = 1978
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "ADW Modula-2 Homepage"
            , href = "http://www.modula2.org"
            }
          ]
        }
      , { name = "Nim"
        , originalAuthors = [ "Andreas Rumpf" ]
        , paradigms =
          [ Paradigm.imperative, Paradigm.objectOriented, Paradigm.functional ]
        , examples = [] : List Example
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
          [ CompilationTarget.machineCode
          , CompilationTarget.language { name = "JavaScript" }
          ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Nim Homepage"
            , href = "https://nim-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Nim Documentation"
            , href = "https://nim-lang.org/documentation.html"
            }
          , { type = ResourceType.sourceRepository
            , title = "Nim GitHub Repository"
            , href = "https://github.com/nim-lang/Nim"
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
          [ Paradigm.functional, Paradigm.imperative, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            Formerly Objective Caml, OCaml is a general-purpose programming
            language in the ML family that supports functional, imperative, and
            object-oriented programming styles. OCaml emphasizes type safety and
            expressiveness while maintaining high performance through native code
            compilation.

            ''
        , yearFirstPublished = 1996
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "OCaml Homepage"
            , href = "https://ocaml.org"
            }
          , { type = ResourceType.documentation
            , title = "OCaml Documentation"
            , href = "https://ocaml.org/docs"
            }
          , { type = ResourceType.book
            , title = "Real World OCaml (book)"
            , href = "https://dev.realworldocaml.org"
            }
          , { title = "Flow"
            , type = ResourceType.notableApplication
            , href = "https://github.com/facebook/flow"
            }
          , { title = "Coq"
            , type = ResourceType.notableApplication
            , href = "https://github.com/coq/coq"
            }
          ]
        }
      , { name = "Oberon"
        , originalAuthors = [ "Niklaus Wirth" ]
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A successor to Modula-2 that introduced object-oriented features while
            maintaining simplicity and efficiency. It was designed alongside the
            Oberon operating system.

            ''
        , yearFirstPublished = 1987
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "ETH Oberon"
            , href = "https://www.inf.ethz.ch/personal/wirth/Oberon"
            }
          ]
        }
      , { name = "Object Pascal"
        , originalAuthors =
          [ "Larry Tesler", "Niklaus Wirth", "Anders Hejlsberg" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            An extension of the Pascal programming language (by way of the obscure
            language Clascal) that supports object-oriented programming. It was
            developed by Apple Computer with the help of Niklaus Wirth and later
            integrated by Borland into Turbo Pascal to become the basis of their
            Delphi product.

            ''
        , yearFirstPublished = 1986
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.documentation
            , title = "Free Pascal (an Object Pascal compiler) Documentation"
            , href = "https://www.freepascal.org/docs.html"
            }
          ]
        }
      , { name = "Objective-C"
        , originalAuthors = [ "Brad Cox", "Tom Love" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A general-purpose, object-oriented programming language that adds
            Smalltalk-style messaging to the C programming language. It was the
            main programming language used by Apple for macOS and iOS development
            before the introduction of Swift.

            ''
        , yearFirstPublished = 1984
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.documentation
            , title = "Objective-C Documentation"
            , href = "https://developer.apple.com/documentation/objectivec"
            }
          , { type = ResourceType.documentation
            , title = "Objective-C Programming Guide"
            , href =
                "https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html"
            }
          ]
        }
      , { name = "Occam"
        , originalAuthors = [ "David May" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A concurrent programming language based on Tony Hoare's Communicating
            Sequential Processes (CSP). Occam was designed by INMOS for
            programming the Transputer parallel processor, featuring channel-based
            message passing and parallel execution primitives.

            ''
        , yearFirstPublished = 1983
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.documentation
            , title = "Occam 2 Reference Manual"
            , href = "https://www.wotug.org/occam/documentation/oc21refman.pdf"
            }
          , { type = ResourceType.homepage
            , title = "WoTUG - World occam and Transputer User Group"
            , href = "https://www.wotug.org/occam"
            }
          ]
        }
      , { name = "Octave"
        , originalAuthors = [ "John W. Eaton" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A high-level language, primarily used for numerical computations.
            Octave is a free and open-source alternative to MATLAB.

            ''
        , yearFirstPublished = 1992
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "GNU Octave"
            , href = "https://www.gnu.org/software/octave"
            }
          , { type = ResourceType.documentation
            , title = "Octave Documentation"
            , href = "https://octave.org/doc/interpreter"
            }
          ]
        }
      , { name = "OpenSCAD"
        , originalAuthors = [ "Clifford Wolf", "Marius Kintel" ]
        , paradigms = [ Paradigm.declarative, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A script-based 3D CAD modeler. OpenSCAD uses a functional
            programming approach to create 3D models, making it particularly
            suited for parametric design and programmatic generation of 3D
            objects.

            ''
        , yearFirstPublished = 2010
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "OpenSCAD Homepage"
            , href = "https://openscad.org/"
            }
          , { type = ResourceType.documentation
            , title = "OpenSCAD User Manual"
            , href = "https://en.wikibooks.org/wiki/OpenSCAD_User_Manual"
            }
          ]
        }
      , { name = "PHP"
        , originalAuthors = [ "Rasmus Lerdorf" ]
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A server-side scripting language originally designed for web
            development. PHP originally stood for "Personal Home Page" but was
            later renamed to "PHP: Hypertext Preprocessor".

            ''
        , yearFirstPublished = 1995
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.weak ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "PHP Homepage"
            , href = "https://www.php.net"
            }
          , { type = ResourceType.documentation
            , title = "PHP Documentation"
            , href = "https://www.php.net/docs.php"
            }
          , { title = "WordPress"
            , type = ResourceType.notableApplication
            , href = "https://github.com/WordPress/WordPress"
            }
          , { title = "Nextcloud"
            , type = ResourceType.notableApplication
            , href = "https://github.com/nextcloud/server"
            }
          ]
        }
      , { name = "PL/I"
        , originalAuthors = [ "IBM" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            Programming Language One, designed by IBM and its user group, SHARE,
            to succeed FORTRAN and COBOL.

            ''
        , yearFirstPublished = 1964
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static ]
        , resources = [] : List Resource
        }
      , { name = "Pascal"
        , originalAuthors = [ "Niklaus Wirth" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
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
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Pascal"
            , href =
                "https://en.wikipedia.org/wiki/Pascal_(programming_language)"
            }
          , { type = ResourceType.implementation
            , title = "Free Pascal"
            , href = "https://www.freepascal.org"
            }
          ]
        }
      , { name = "Perl"
        , originalAuthors = [ "Larry Wall" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A general-purpose language originally designed for text processing.
            Perl is known for its flexibility and its ability to easily create
            domain-specific languages.

            ''
        , yearFirstPublished = 1987
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Perl Homepage"
            , href = "https://www.perl.org"
            }
          , { type = ResourceType.documentation
            , title = "Perl Documentation"
            , href = "https://perldoc.perl.org"
            }
          ]
        }
      , { name = "Pony"
        , originalAuthors = [ "Sebastian Blessing", "Sylvan Clebsch" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.functional ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content =
                    ''
                    actor Main
                      new create(env: Env) =>
                        env.out.print("Hello, world!")
                    ''
                }
              ]
            : List Example
        , description =
            ''

            An object-oriented programming language designed for writing safe,
            high-performance actor-based programs. Pony emphasizes
            capabilities-secure type system and data-race freedom through its
            reference capabilities system.

            ''
        , yearFirstPublished = 2015
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.nominal ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Pony Homepage"
            , href = "https://www.ponylang.io"
            }
          , { type = ResourceType.tutorial
            , title = "Pony Tutorial"
            , href = "https://tutorial.ponylang.io"
            }
          , { type = ResourceType.documentation
            , title = "Pony Documentation"
            , href = "https://www.ponylang.io/learn"
            }
          ]
        }
      , { name = "Prolog"
        , originalAuthors = [ "Alain Colmerauer", "Philippe Roussel" ]
        , paradigms = [ Paradigm.logic, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A logic programming language based on first-order predicate
            calculus. Programs consist of facts and rules, and computation
            proceeds by making logical queries against this knowledge base.

            ''
        , yearFirstPublished = 1972
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.implementation
            , title = "SWI-Prolog (a Prolog implementation)"
            , href = "https://swi-prolog.org"
            }
          , { type = ResourceType.tutorial
            , title = "Prolog Tutorial"
            , href = "https://www.swi-prolog.org/pldoc/man?section=quickstart"
            }
          , { type = ResourceType.implementation
            , title = "GNU Prolog (a Prolog implementation)"
            , href = "http://gprolog.org"
            }
          ]
        }
      , { name = "PureScript"
        , originalAuthors = [ "Phil Freeman" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A strongly-typed functional programming language that compiles to
            JavaScript. PureScript is similar to Haskell but designed specifically
            for the web platform, featuring row polymorphism, type classes, and
            strict evaluation.

            ''
        , yearFirstPublished = 2013
        , compilationTargets =
          [ CompilationTarget.language { name = "JavaScript" } ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "PureScript Homepage"
            , href = "https://www.purescript.org"
            }
          , { type = ResourceType.documentation
            , title = "PureScript Documentation"
            , href = "https://github.com/purescript/documentation"
            }
          , { type = ResourceType.playground
            , title = "Try PureScript"
            , href = "https://try.purescript.org"
            }
          ]
        }
      , { name = "Pyret"
        , originalAuthors =
          [ "Joe Gibbs Politz"
          , "Benjamin Lerner"
          , "Daniel Patterson"
          , "Dorai Sitaram"
          ]
        , paradigms = [ Paradigm.functional, Paradigm.objectOriented ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content =
                    ''
                    print("Hello, World!")
                    ''
                }
              ]
            : List Example
        , description =
            ''

            A programming language designed for teaching computer science,
            featuring built-in testing, tables, and image manipulation. Pyret
            emphasizes clear error messages and integrates features from both
            functional and object-oriented programming.

            ''
        , yearFirstPublished = 2012
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Pyret Homepage"
            , href = "https://www.pyret.org"
            }
          , { type = ResourceType.documentation
            , title = "Pyret Documentation"
            , href = "https://www.pyret.org/docs"
            }
          , { type = ResourceType.playground
            , title = "Online Editor"
            , href = "https://code.pyret.org"
            }
          ]
        }
      , { name = "Python"
        , originalAuthors = [ "Guido van Rossum" ]
        , paradigms =
          [ Paradigm.imperative, Paradigm.objectOriented, Paradigm.functional ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content =
                    ''
                    print("Hello, World!")
                    ''
                }
              ]
            : List Example
        , description =
            ''

            A high-level, general-purpose programming language emphasizing code
            readability with its notable use of significant whitespace. Python
            supports multiple programming paradigms and features a comprehensive
            (frequently referred to as "batteries-included") standard library.

            ''
        , yearFirstPublished = 1991
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Python Homepage"
            , href = "https://www.python.org"
            }
          , { type = ResourceType.documentation
            , title = "Python Documentation"
            , href = "https://docs.python.org"
            }
          , { title = "Django"
            , type = ResourceType.popularLibrary
            , href = "https://github.com/django/django"
            }
          , { title = "Ansible"
            , type = ResourceType.notableApplication
            , href = "https://github.com/ansible/ansible"
            }
          ]
        }
      , { name = "QBasic"
        , originalAuthors = [ "Microsoft" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            An IDE and interpreter for a version of BASIC developed by
            Microsoft. It was included with MS-DOS and was many programmers' first
            introduction to programming.

            ''
        , yearFirstPublished = 1991
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.implementation
            , title = "QB64 (a modern implementation of QBASIC)"
            , href = "https://qb64.com"
            }
          ]
        }
      , { name = "R"
        , originalAuthors = [ "Ross Ihaka", "Robert Gentleman" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''
            A language for statistical computing and data visualization.
            ''
        , yearFirstPublished = 1993
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "R Homepage"
            , href = "https://www.r-project.org"
            }
          , { type = ResourceType.documentation
            , title = "R Documentation"
            , href = "https://www.rdocumentation.org"
            }
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
        , paradigms = [ Paradigm.functional, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A general-purpose, multi-paradigm programming language in the
            Lisp/Scheme family. Racket is designed to be a platform for
            programming language design, implementation, and learning.

            ''
        , yearFirstPublished = 1995
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Racket Homepage"
            , href = "https://racket-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Racket Documentation"
            , href = "https://docs.racket-lang.org"
            }
          ]
        }
      , { name = "Raku"
        , originalAuthors = [ "Larry Wall" ]
        , paradigms = [ Paradigm.objectOriented, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A multi-paradigm language formerly known as Perl 6, designed to be
            more consistent and modern than Perl 5.

            ''
        , yearFirstPublished = 2015
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.dynamic, TypingSystem.gradual, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Raku Homepage"
            , href = "https://raku.org"
            }
          , { type = ResourceType.documentation
            , title = "Raku Documentation"
            , href = "https://docs.raku.org"
            }
          ]
        }
      , { name = "ReasonML"
        , originalAuthors = [ "Jordan Walke" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A syntax and toolchain for OCaml, created at Facebook. ReasonML
            provides a more familiar C-style syntax while leveraging OCaml's type
            system and compilation.

            ''
        , yearFirstPublished = 2016
        , compilationTargets =
          [ CompilationTarget.machineCode
          , CompilationTarget.language { name = "JavaScript" }
          ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "ReasonML Homepage"
            , href = "https://reasonml.github.io"
            }
          , { type = ResourceType.documentation
            , title = "ReasonML Documentation"
            , href = "https://reasonml.github.io/docs/en/what-and-why"
            }
          , { type = ResourceType.playground
            , title = "Try ReasonML Online"
            , href = "https://reasonml.github.io/en/try"
            }
          , { type = ResourceType.sourceRepository
            , title = "ReasonML GitHub Repository"
            , href = "https://github.com/reasonml/reason"
            }
          ]
        }
      , { name = "Rebol"
        , originalAuthors = [ "Carl Sassenrath" ]
        , paradigms = [ Paradigm.functional, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A lightweight messaging language designed for distributed computing
            and network communications, with a focus on human-readable syntax.
            Its author, Carl Sassenrath, claims its greatest strength is its
            ability to easily create domain-specific languages.

            ''
        , yearFirstPublished = 1997
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Rebol Homepage"
            , href = "https://www.rebol.com"
            }
          , { type = ResourceType.documentation
            , title = "Rebol Documentation"
            , href = "https://www.rebol.com/docs.html"
            }
          ]
        }
      , { name = "Red"
        , originalAuthors = [ "Nenad Rakočević" ]
        , paradigms = [ Paradigm.functional, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A next-generation programming language strongly inspired by Rebol but
            with a focus on cross-compilation, concurrency and high performance.

            ''
        , yearFirstPublished = 2011
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Red Homepage"
            , href = "https://www.red-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Red Documentation"
            , href = "https://docs.red-lang.org"
            }
          , { type = ResourceType.sourceRepository
            , title = "GitHub Repository"
            , href = "https://github.com/red/red"
            }
          ]
        }
      , { name = "Roc"
        , originalAuthors = [ "Richard Feldman" ]
        , paradigms = [ Paradigm.functional ]
        , examples =
              [ { type = ExampleType.map
                , content =
                    ''
                    credits = List.map(songs, |song|
                        "Performed by ''${song.artist}"
                    )
                    ''
                }
              ]
            : List Example
        , description =
            ''

            A fast, friendly, functional programming language focusing on
            zero-cost abstractions, great error messages, and first-class support
            for building command-line interfaces and network services.

            ''
        , yearFirstPublished = 2020
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Roc Homepage"
            , href = "https://www.roc-lang.org"
            }
          , { type = ResourceType.tutorial
            , title = "Roc Documentation"
            , href = "https://www.roc-lang.org/tutorial"
            }
          ]
        }
      , { name = "Ruby"
        , originalAuthors = [ "Yukihiro \"Matz\" Matsumoto" ]
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content = "puts \"Hello World!\""
                }
              ]
            : List Example
        , description =
            ''

            A dynamic, object-oriented language emphasizing simplicity and
            productivity. Ruby is widely known for the web development framework
            Ruby on Rails, upon which many successful companies (Twitter, GitHub)
            were built.

            ''
        , yearFirstPublished = 1995
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Ruby Homepage"
            , href = "https://ruby-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Ruby Documentation"
            , href = "https://www.ruby-lang.org/en/documentation"
            }
          , { type = ResourceType.playground
            , title = "Try Ruby (browser REPL)"
            , href = "https://try.ruby-lang.org"
            }
          , { title = "GitLab"
            , type = ResourceType.notableApplication
            , href = "https://github.com/gitlabhq/gitlabhq"
            }
          , { title = "Homebrew"
            , type = ResourceType.notableApplication
            , href = "https://github.com/Homebrew/brew"
            }
          ]
        }
      , { name = "Rust"
        , originalAuthors = [ "Graydon Hoare" ]
        , paradigms = [ Paradigm.imperative, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A systems programming language focused on safety, concurrency, and
            performance, originally developed at Mozilla. Rust is notable for its
            compile-time memory safety guarantees through a unique ownership
            system.

            ''
        , yearFirstPublished = 2012
        , compilationTargets =
          [ CompilationTarget.machineCode, CompilationTarget.webassembly ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Rust Homepage"
            , href = "https://www.rust-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Rust Documentation"
            , href = "https://doc.rust-lang.org"
            }
          , { type = ResourceType.book
            , title = "The Rust Programming Language (book)"
            , href = "https://doc.rust-lang.org/book"
            }
          , { title = "Servo"
            , type = ResourceType.notableApplication
            , href = "https://github.com/servo/servo"
            }
          , { title = "ripgrep"
            , type = ResourceType.notableApplication
            , href = "https://github.com/BurntSushi/ripgrep"
            }
          ]
        }
      , { name = "SNOBOL"
        , originalAuthors =
          [ "Ralph Griswold", "Ivan Polonsky", "David Farber" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            StriNg Oriented symBOlic Language, designed for text processing.
            SNOBOL had support for "patterns", which were more powerful than
            regular expressions. SNOBOL was used for text processing in the 1960s
            and 1970s, but was largely supplanted by Perl and AWK.

            ''
        , yearFirstPublished = 1962
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: SNOBOL"
            , href = "https://en.wikipedia.org/wiki/SNOBOL"
            }
          ]
        }
      , { name = "SQL"
        , originalAuthors = [ "Donald D. Chamberlin", "Raymond F. Boyce" ]
        , paradigms = [ Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            Structured Query Language, a domain-specific language designed for
            managing and querying relational databases. It has become the standard
            language for relational database management systems, used in
            PostgreSQL, MySQL, Oracle, and other database systems.

            ''
        , yearFirstPublished = 1974
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.languageSpecification
            , title = "SQL Standard"
            , href = "https://www.iso.org/standard/76583.html"
            }
          ]
        }
      , { name = "Scala"
        , paradigms = [ Paradigm.functional, Paradigm.objectOriented ]
        , originalAuthors = [ "Martin Odersky" ]
        , examples = [] : List Example
        , description =
            ''

            Originally SCAlable LAnguage, Scala is a language combining
            object-oriented and functional programming running on the JVM.

            ''
        , yearFirstPublished = 2004
        , compilationTargets =
          [ CompilationTarget.jvmBytecode
          , CompilationTarget.language { name = "JavaScript" }
          ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Scala Homepage"
            , href = "https://www.scala-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Scala Documentation"
            , href = "https://docs.scala-lang.org"
            }
          , { title = "Apache Spark"
            , type = ResourceType.notableApplication
            , href = "https://github.com/apache/spark"
            }
          ]
        }
      , { name = "Scheme"
        , originalAuthors = [ "Guy L. Steele", "Gerald Jay Sussman" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A dialect of Lisp emphasizing simplicity and minimalism, widely used
            in computer science education and research. There are several
            specifications for the Scheme language, the latest of which is R7RS
            for which the "small language" design was completed in 2013, but the
            "large language" design is unfinished.

            ''
        , yearFirstPublished = 1975
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Scheme (programming language)"
            , href =
                "https://en.wikipedia.org/wiki/Scheme_(programming_language)"
            }
          , { type = ResourceType.languageSpecification
            , title = "R7RS Website"
            , href = "https://r7rs.org"
            }
          ]
        }
      , { name = "Scratch"
        , originalAuthors = [ "Mitchel Resnick" ]
        , paradigms = [ Paradigm.imperative, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A block-based visual programming language and online community
            targeted primarily at children. Scratch allows users to create
            programs by snapping together code blocks in a drag-and-drop
            interface.

            ''
        , yearFirstPublished = 2003
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Scratch Homepage"
            , href = "https://scratch.mit.edu"
            }
          ]
        }
      , { name = "Seed7"
        , originalAuthors = [ "Thomas Mertes" ]
        , paradigms =
          [ Paradigm.imperative, Paradigm.objectOriented, Paradigm.declarative ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                $ include "seed7_05.s7i";

                const proc: main is func
                  begin
                    writeln("Hello, world!");
                  end func;
                ''
            }
          ]
        , description =
            ''

            Seed7 is a general-purpose programming language designed by Thomas
            Mertes, aiming to be expressive and extensible. Seed7 has a unique
            approach to extensibility, allowing programmers to define new
            syntax and control structures. The language includes static typing
            with type inference, support for Unicode, and a comprehensive
            standard library.

            ''
        , yearFirstPublished = 2005
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Seed7 Homepage"
            , href = "https://seed7.net/"
            }
          , { type = ResourceType.sourceRepository
            , title = "Seed7 Source Repository"
            , href = "https://github.com/ThomasMertes/seed7"
            }
          , { type = ResourceType.documentation
            , title = "Seed7 Manual"
            , href = "https://seed7.net/manual/index.htm"
            }
          , { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Seed7"
            , href = "https://en.wikipedia.org/wiki/Seed7"
            }
          ]
        }
      , { name = "Self"
        , originalAuthors = [ "David Ungar", "Randall Smith" ]
        , paradigms = [ Paradigm.objectOriented ]
        , examples =
          [ { type = ExampleType.helloWorld
            , content =
                ''
                'Hello, World!' print.
                ''
            }
          ]
        , description =
            ''

            Self is a prototype-based object-oriented programming language that
            pioneered many concepts used in modern languages like JavaScript. Self
            was based on Smalltalk, but designed to further object-oriented
            programming language research. It was originally designed at Xerox
            PARC, but most of Self's development was done at Sun Microsystems.

            ''
        , yearFirstPublished = 1987
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Self (programming language)"
            , href = "https://en.wikipedia.org/wiki/Self_(programming_language)"
            }
          ]
        }
      , { name = "Shen"
        , originalAuthors = [ "Mark Tarver" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A functional programming language with an integrated type system
            based on sequent calculus. Shen is notable for its pattern matching,
            optional typing via Sequent Calculus, and Lisp-like syntax.

            ''
        , yearFirstPublished = 2011
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing =
          [ TypingSystem.static, TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Shen Language Homepage"
            , href = "https://shenlanguage.org/"
            }
          , { type = ResourceType.documentation
            , title = "Shen Documentation"
            , href = "https://shenlanguage.org/learn-shen/index.html"
            }
          ]
        }
      , { name = "Simula"
        , originalAuthors = [ "Ole-Johan Dahl", "Kristen Nygaard" ]
        , paradigms = [ Paradigm.objectOriented ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content =
                    ''
                    Begin
                        OutText ("Hello, World!");
                        Outimage;
                    End;
                    ''
                }
              ]
            : List Example
        , description =
            ''

            A simulation programming language, Simula is considered the first
            object-oriented programming language. It is an approximate superset of
            ALGOL 60.

            ''
        , yearFirstPublished = 1965
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Simula"
            , href = "https://en.wikipedia.org/wiki/Simula"
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
        , paradigms = [ Paradigm.objectOriented ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content =
                    ''
                    Transcript show: 'Hello, World!'.
                    ''
                }
              ]
            : List Example
        , description =
            ''

            Developed at Xerox PARC, Smalltalk is one of the first pure
            object-oriented programming languages, where all data, even basic
            types like numbers, are objects. Smalltalk was very influential in the
            design of later object-oriented programming languages.

            ''
        , yearFirstPublished = 1972
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.implementation
            , title = "Squeak Homepage (Smalltalk Implementation)"
            , href = "https://squeak.org"
            }
          , { type = ResourceType.implementation
            , title = "Pharo Homepage (Smalltalk Implementation)"
            , href = "https://pharo.org"
            }
          ]
        }
      , { name = "Souffle"
        , originalAuthors = [ "Bernhard Scholz" ]
        , paradigms = [ Paradigm.logic, Paradigm.declarative ]
        , examples = [] : List Example
        , description =
            ''

            A logic programming language inspired by Datalog but with extensions
            beyond classical Datalog. Souffle was designed for large-scale static
            program analysis and compiles to parallel C++ for high performance.

            ''
        , yearFirstPublished = 2016
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Souffle Homepage"
            , href = "https://souffle-lang.github.io/"
            }
          , { type = ResourceType.documentation
            , title = "Souffle Documentation"
            , href = "https://souffle-lang.github.io/docs.html"
            }
          ]
        }
      , { name = "Standard ML (SML)"
        , originalAuthors = [ "Robin Milner", "Mads Tofte", "Robert Harper" ]
        , paradigms = [ Paradigm.imperative, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            An attempt to standardize ML implementations, Standard ML (SML) is a
            general-purpose functional programming language that features static
            typing, type inference, pattern matching, and a sophisticated module
            system. SML influenced many modern functional languages including
            OCaml, F#, and Scala.

            ''
        , yearFirstPublished = 1983
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Standard ML Family GitHub"
            , href = "https://smlfamily.github.io"
            }
          , { type = ResourceType.implementation
            , title = "SML/NJ (a compiler for SML)"
            , href = "https://www.smlnj.org"
            }
          , { type = ResourceType.implementation
            , title = "MLton (a compiler for SML)"
            , href = "http://mlton.org"
            }
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
        , paradigms = [ Paradigm.objectOriented, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A modern programming language developed by Apple as a replacement for
            Objective-C. Swift is primarily used to create apps for iOS, macOS,
            and other Apple operating systems.

            ''
        , yearFirstPublished = 2014
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Swift Homepage"
            , href = "https://www.swift.org"
            }
          , { type = ResourceType.documentation
            , title = "Swift Documentation"
            , href = "https://docs.swift.org"
            }
          , { type = ResourceType.book
            , title = "The Swift Programming Language (book)"
            , href =
                "https://docs.swift.org/swift-book/documentation/the-swift-programming-language"
            }
          , { type = ResourceType.documentation
            , title = "Apple Documentation"
            , href = "https://developer.apple.com/swift"
            }
          ]
        }
      , { name = "Tcl"
        , originalAuthors = [ "John Ousterhout" ]
        , paradigms = [ Paradigm.imperative ]
        , examples =
              [ { type = ExampleType.helloWorld
                , content =
                    ''
                    puts "Hello, World!"
                    ''
                }
              , { type = ExampleType.sum
                , content =
                    ''
                    set sum [expr 1+2+3+4+5]
                    puts "The sum of the numbers 1..5 is $sum."
                    ''
                }
              ]
            : List Example
        , description =
            ''

            Tool Command Language, a scripting language commonly used for rapid
            prototyping, scripted applications, GUIs, and testing.  Tcl, often
            pronounced "tickle", has a unique typing system in which all data may
            be manipulated as strings. Tcl is often paired with Tk, a GUI toolkit.

            ''
        , yearFirstPublished = 1988
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Tcl Homepage"
            , href = "https://www.tcl-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Tcl Documentation"
            , href = "https://www.tcl-lang.org/doc"
            }
          ]
        }
      , { name = "Turbo Pascal"
        , originalAuthors =
          [ "Borland Software Corporation", "Anders Hejlsberg" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A software development system that includes a compiler and an
            integrated development environment (IDE) for the Pascal programming
            language on CP/M, CP/M-86, and MS-DOS. Turbo Pascal was developed by
            Borland and was popular in the 1980s and early 1990s.

            ''
        , yearFirstPublished = 1983
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: Turbo Pascal"
            , href = "https://en.wikipedia.org/wiki/Turbo_Pascal"
            }
          ]
        }
      , { name = "TypeScript"
        , originalAuthors = [ "Anders Hejlsberg" ]
        , paradigms =
          [ Paradigm.objectOriented, Paradigm.functional, Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A strict syntactical superset of JavaScript that adds optional static
            typing. Developed by Microsoft to enable JavaScript development at
            scale. TypeScript has wide support in modern browser application
            development.

            ''
        , yearFirstPublished = 2012
        , compilationTargets =
          [ CompilationTarget.language { name = "JavaScript" } ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.structural ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "TypeScript Homepage"
            , href = "https://www.typescriptlang.org"
            }
          , { type = ResourceType.documentation
            , title = "TypeScript Documentation"
            , href = "https://www.typescriptlang.org/docs"
            }
          , { type = ResourceType.book
            , title = "The TypeScript Handbook"
            , href = "https://www.typescriptlang.org/docs/handbook/intro.html"
            }
          ]
        }
      , { name = "Unison"
        , originalAuthors = [ "Paul Chiusano", "Rúnar Bjarnason" ]
        , paradigms = [ Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A modern functional programming language that emphasizes distributed
            computing and features a unique content-addressed codebase. Unison
            aims to simplify code collaboration and versioning.

            ''
        , yearFirstPublished = 2019
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Unison Homepage"
            , href = "https://www.unison-lang.org"
            }
          , { type = ResourceType.documentation
            , title = "Unison Documentation"
            , href = "https://www.unison-lang.org/docs"
            }
          , { type = ResourceType.homepage
            , title = "Unison Discord"
            , href = "https://unison-lang.org/discord"
            }
          , { type = ResourceType.homepage
            , title = "Unison Blog"
            , href = "https://www.unison-lang.org/blog"
            }
          , { type = ResourceType.homepage
            , title = "Unison Cloud"
            , href = "https://www.unison.cloud"
            }
          , { type = ResourceType.sourceRepository
            , title = "Unison GitHub Repository"
            , href = "https://github.com/unisonweb/unison"
            }
          ]
        }
      , { name = "V"
        , originalAuthors = [ "Alexander Medvednikov" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            A systems programming language, very similar to Go. V emphasizes
            safety, performance, and simplicity with fast compilation, memory
            safety without garbage collection, and C-style syntax.

            V has a few notable projects, including Vinix and Gitly, a OS
            kernel and GitHub/Gitlab clone each written in V, respectively.            

            ''
        , yearFirstPublished = 2019
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing = [ TypingSystem.static, TypingSystem.strong ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "V Programming Language"
            , href = "https://vlang.io"
            }
          , { type = ResourceType.documentation
            , title = "V Documentation"
            , href = "https://docs.vlang.io"
            }
          , { type = ResourceType.sourceRepository
            , title = "V GitHub Repository"
            , href = "https://github.com/vlang/v"
            }
          , { type = ResourceType.playground
            , title = "V Playground (online editor)"
            , href = "https://play.vlang.io"
            }
          ]
        }
      , { name = "Visual Basic"
        , originalAuthors = [ "Microsoft" ]
        , paradigms = [ Paradigm.imperative, Paradigm.objectOriented ]
        , examples = [] : List Example
        , description =
            ''

            A version of BASIC developed by Microsoft for its COM programming
            model and Windows GUI development. The latest version is Visual Basic
            .NET.

            ''
        , yearFirstPublished = 1991
        , compilationTargets =
          [ CompilationTarget.machineCode, CompilationTarget.cil ]
        , typing = [ TypingSystem.static ]
        , resources =
          [ { type = ResourceType.documentation
            , title = "Visual Basic Documentation"
            , href = "https://learn.microsoft.com/en-us/dotnet/visual-basic"
            }
          ]
        }
      , { name = "Zig"
        , originalAuthors = [ "Andrew Kelley" ]
        , paradigms = [ Paradigm.imperative, Paradigm.functional ]
        , examples = [] : List Example
        , description =
            ''

            A general-purpose programming language with an emphasis on being
            explicit. To this end, Zig makes features that are usually implicit,
            such as memory allocations, explicit. Zig supports incremental
            adoption into C/C++ codebases.

            ''
        , yearFirstPublished = 2016
        , compilationTargets = [ CompilationTarget.machineCode ]
        , typing =
          [ TypingSystem.static, TypingSystem.strong, TypingSystem.inferred ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "Zig Homepage"
            , href = "https://ziglang.org"
            }
          , { type = ResourceType.sourceRepository
            , title = "Compiler repository"
            , href = "https://github.com/ziglang/zig"
            }
          ]
        }
      , { name = "csh"
        , originalAuthors = [ "Bill Joy" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            The C shell, a Unix shell with C-like syntax. Developed at UC
            Berkeley, csh introduced features like aliases, command history, and
            job control that influenced later shells.

            ''
        , yearFirstPublished = 1978
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: C shell"
            , href = "https://en.wikipedia.org/wiki/C_shell"
            }
          ]
        }
      , { name = "ksh"
        , originalAuthors = [ "David Korn" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            The Korn shell, a Unix shell developed at AT&T Bell Laboratories.
            ksh combines features from the Bourne shell and C shell, adding
            command-line editing and other improvements that influenced Bash.

            ''
        , yearFirstPublished = 1983
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: KornShell"
            , href = "https://en.wikipedia.org/wiki/KornShell"
            }
          ]
        }
      , { name = "tcsh"
        , originalAuthors = [ "Ken Greer" ]
        , paradigms = [ Paradigm.imperative ]
        , examples = [] : List Example
        , description =
            ''

            An enhanced version of the C shell (csh) with command-line editing,
            programmable command completion, and spelling correction. tcsh is the
            default shell on FreeBSD.

            ''
        , yearFirstPublished = 1981
        , compilationTargets = [ CompilationTarget.interpreted ]
        , typing = [ TypingSystem.dynamic ]
        , resources =
          [ { type = ResourceType.homepage
            , title = "tcsh Homepage"
            , href = "https://www.tcsh.org/"
            }
          , { type = ResourceType.wikipediaEntry
            , title = "Wikipedia: tcsh"
            , href = "https://en.wikipedia.org/wiki/Tcsh"
            }
          ]
        }
      ]
    : List Language
