foundational principles
=======================

100% built from the ground-up for fast, /incremental/ builds

 1. persistent build daemon
 1. watches filesystem
     - able to intelligently determine relationships between all modules being watched
     - leverages static type systems where able (e.g. read java .class files for per-class incremental builds)
     - rebuilds builds dependent modules/code
     - automatically update/restart daemons when code changes

goals
=======================
     
  - build an incremental build system that is generic enough to support various languages, environments
       - java
         - simple 'jar' libraries
         - java 'double-clickable' jars
         - jee artifacts ('war', 'ejb-jar', 'ear')
       - HTML/web & related technologies
         - static analysis tools (e.g. jshint)
         - node
         - optimization/compilation tools
           - coffeescript
       - .net
       - C/Cpp, etc
         - 'make' replacement
       - scripting languages
         - ruby/rails
         - python
         - etc
       - different kinds of artifact repositories
         - maven central, etc
         - cpan
         - npm
  - should be more-or-less a conceptual sub-set of maven ... should be able to generate an equivalent maven pom (for IDE & build tooling integration)
  - be as efficient as eclipse's incremental compiler
  - easy to integrate with editors & IDEs
    - exposes compile/processing errors with file names and line numbers
    - exposes refactoring functionality where able
    - exposes processing state
    - relays changes on filesystem

opinions
==================================== 

  - keep modules simple ... no "src/test/*", /ONLY/ one artifact per module, per build
  - 'version' is a useless concept ... a build is what we actually work with, what we depend on
  - no sneaky automatic downloading of binaries ... iq always asks before downloading things
  - a program should always validate it's input (i.e. no "-DhopeAPluginKnowsAboutThisFlag" settings that are silently ignored")
  - simple tools are nice ... let's make it easy to use with a simple text editor
  - no mind-numbing, disk-frying, time-wasting incessant "mvn install"'ing of locally-built artifacts to a "local repository"
  - a build system should be helpful ... for most common errors, it should be able to give useful suggestions & analysis, if the user desires

notes
==========

I'm envisioning something of a cross between:
  - maven (the good parts)
  - hudson/jenkins (the good parts)
  - eclipse (the good parts)

The final product will drastically simplify the way software is built & delivered, across all the workflows in the entire code/build/test/deploy lifecycle
