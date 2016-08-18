![logo](bph16_logo.png)

# Budapest Haskell Hackathon 2016 (2016–08–07)


Our meetup group held a two day hackathon on the 6th and 7th of August. During the event we had four longer presentations at the event, and four lightning talks. You can find the slides, and links to the videos here, but for more general information about the hackathon please visit the event's [wiki][wiki].

## 🎙️ Presentations 🎙️

### Dániel Vigovszky: Haskell at Prezi

_Abstract_:
> “Prezi uses Haskell in its document model related projects. In this talk first we will explain the domain and show the problems we wanted to solve. Following that we will first show the unique build environment we created to be able to work efficiently on complex Haskell codebases. Then we will look into our extensive use of QuickCheck, how it's used and what we learned.”

More info:

* [slides](https://prezi.com/p/j0dehuddmj1d/)
* [video][yt_prezi]

### Máté Karácsony: Zeldspar: The Road to Epiphany

_Abstract_:
> “Zeldspar is a domain-specific language to implement digital signal processing pipelines. It's an implementation of the Ziria language (by Microsoft Research) on the top of Feldspar (an EDSL written in Haskell for signal processing, developed at Chalmers in Göteborg, Sweden). In this talk I will present how Zeldspar is built on the top of a deep language stack, and I also show how it can be compiled to run efficiently on many-core devices such as Adapteva's Epiphany chips and Parallella boards.”

More info:

* [slides](presentations/MKaracsony_Zeldspar/Zeldspar_the_road_to_epiphany.pdf)
* [video][yt_zeldspar]

### András Kovács: A tour of GHC 8 features

_Abstract:_
> “GHC 8 brought us a number of new features. This talk focuses on the changes to the type system (new kind system, explicit type applications, injective families) and presents some use cases and idioms that were impossible or unwieldy with previous GHC releases. We also touch on Strict Haskell, stack tracing, custom type errors, record field overloading, and their current usability.”

More info:

* [presentation Haskell files](presentations/AKovacs_GHC8)
* [video][yt_ghc8]

### Dániel Berényi: Selected use cases of structured recursion schemes

_Abstract:_
> "We show how the simplest structured recursion schemes can drive generic tree manipulations. Catamorphisms perform bottom-up, while Anamorphisms do the opposite (top-down) recursive traversals. The transformations applied to the trees are completely separated from the traversal in the form of algebras and coalgebras. We show how these methods can be used to do simple manipulations to a small Embedded Domain Specific Language. We briefly review their potential applications in High-Performance Computing applications.”

More info:

* [slides](presentations/DBerenyi_Recursion_schemes/DBerenyi_HaskellHackathon.pdf)
* [example files](presentations/DBerenyi_Recursion_schemes/)
* [video][yt_gpulab]


## ⚡️Ligthining talks⚡️

### Andor Pénzes: OO in Haskell
_Description:_ A quick experiment to implement OO methods in Haskell. You can follow the progression through the commits.

More info:

* [github page][oo_haskell]
* [video][yt_oo]

### Frantisek Kocun: Howerpoint: CLI presentation slides is Haskell
_Description:_ Use GHCi to display slides for your presentations.

More info:

* [github page][howerpoint]
* [video][yt_hower]

### Boldizsár Német: Haskell tools demo
_Description:_ A new refractoring tool for Haskell.
**Contributors are welcome! If you know to write plugins for text editors (e.g. Emacs, Vim), or IDEs and want to help please contact Boldizsár!**

More info:

* [slides](lightning_talks/BNemeth_Haskell_tools.pdf)
* [website][htoolsweb]
* [github page][htoolsgit]
* [video][yt_htools]

### Péter Divánszky: x86–64 code generation is Haskell demo
_Description:_ Generate x86 assembly code from Haskell.

More info:

* [github page][x86]
* [video][yt_x86]

[wiki]: https://wiki.haskell.org/Budapest_Hackathon_2016
[oo_haskell]: https://github.com/andorp/oo-haskell
[howerpoint]: https://github.com/fokot/howerpoint
[htoolsweb]: http://haskelltools.org
[htoolsgit]: https://github.com/haskell-tools/haskell-tools
[x86]: https://github.com/divipp/x86-64
[yt_prezi]: https://www.youtube.com/watch?v=5fnQWaIS30A
[yt_zeldspar]: https://www.youtube.com/watch?v=GY5SxLkVyp4
[yt_gpulab]: https://www.youtube.com/watch?v=0Od_smjZ_jU
[yt_ghc8]: https://www.youtube.com/watch?v=cCGqV90jyOc
[yt_oo]: https://www.youtube.com/watch?v=vw9PKN7EwTs
[yt_hower]: https://www.youtube.com/watch?v=C88Og_lX22E
[yt_htools]: https://www.youtube.com/watch?v=wpNxuR-XgNQ
[yt_x86]: https://www.youtube.com/watch?v=H8gPOHb2A-0
