scala-99
========

Ninety-Nine Scala Problems
-------------------

This is a translation of *Ninety-Nine Haskell Problems* which are themselves
translations of Ninety-Nine Lisp Problems which are themselves translations
of Ninety-Nine Prolog Problems.

See [Ninety-Nine Haskell Problems](http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems)
for more info.
Also see [Phil Gold's S-99](http://aperiodic.net/phil/scala/s-99/) for a more Scala-specific definition of the problems.

How to compile/edit/use
-------------------

The build manager the project uses is [sbt](http://www.scala-sbt.org/).

The suggested editor is [Scala IDE](http://scala-ide.org/) (an Eclipse variant/plugin).

To use the project from Eclipse you first need to have the
[sbteclipse](https://github.com/typesafehub/sbteclipse) *sbt plugin* installed
(added to your user's plugin definition file). Then generate project files from sbt:

    $ sbt
    > eclipse

You may now import the project's folder in Eclipse with the *Import Wizard* as an *Existing Project*.
