## What is findclass?

findclass is a little tool that facilitates navigating around source code in a
lightweight, mostly-works manner. It's kind of like etags and friends, but has
the all-important benefit of being easily hackable by its author.

It "parses" source files looking for class, interface, enum, trait, etc.
declarations and notes the file and line at which they were declared. This
information can then be used to navigate from the occurrence of a type name in
one source file, to its definition. Or simply to navigate to a desired class
definition by typing its name.

One benefit is that you can use findclass with zero prior configuration and it
will scan the project "surrounding" the current file for the desired
definition. This is very handy when you just unpacked some random source code
and are trying to grok some bit of it, but don't really want to go to all the
trouble of slurping it into a proper IDE.

Additionally, with some very lightweight configuration, it can be used to
easily navigate projects that you work on frequently, if you're someone who
prefers to do most of their work in old school text editors (like vi or emacs)
rather than a proper IDE.

findclass is just operating with strings, it knows nothing of scopes and types,
so don't be surprised if it doesn't always find the right class. Its goal in
life is to get the job done most of the time, and when it doesn't quite work,
you can expend a little effort to get where you want. Remember, the alternative
is to run a real IDE which will do the right thing all the time, for the low
low price of a 50% overall reduction in productivity.

## Quick start

1. Download [findclass.jar] and [findclass.el] and stick them somewhere.

2. Put the following in your .emacs file:

        (load "WHEREVER/findclass.el")
        (set-variable 'findclass-jar "WHEREVER/findclass.jar")

        ;; obviously tweak these key-combinations to taste
        (defun findclass-java-mode-hook ()
          (define-key java-mode-map "\C-c\C-i" 'import-class-at-symbol)
          (define-key java-mode-map "\C-c\C-j" 'open-class-at-symbol)
          )
        (add-hook 'java-mode-hook 'findclass-java-mode-hook);

        ;; if you use scala or actionscript, throw those in as well
        (defun findclass-scala-mode-hook ()
          (define-key scala-mode-map "\C-c\C-i" 'import-class-at-symbol)
          (define-key scala-mode-map "\C-c\C-j" 'open-class-at-symbol)
          )
        (add-hook 'scala-mode-hook 'findclass-scala-mode-hook);
        (defun findclass-actionscript-mode-hook ()
          (define-key actionscript-mode-map "\C-c\C-i" 'import-class-at-symbol)
          (define-key actionscript-mode-map "\C-c\C-j" 'open-class-at-symbol)
          )
        (add-hook 'actionscript-mode-hook 'findclass-actionscript-mode-hook);

3. Edit a file and feel the power of your new navigational abilities.

## Advanced usage

If you want to use findclass for more than trivial navigation, you can tell it
where to find source code for projects upon which a given project depends. You
do this by creating a `.findclass.path` file at the top-level of your project.
It should contain paths to other projects whose source will be searched. For
example:

    ~/projects/samskivert
    ~/ops/guava/guava

Note that when findclass finds a `.findclass.path` file, it will generate a
cache file containing the information for all of the classes in the listed
projects. This makes subsequent lookups very fast, but the cache file may
become out of date. findclass does not automatically regenerate the cache file,
you can instruct it to do so by prefixing your emacs key binding with `ctrl-u`,
this will first rebuild the cache and then seek the specified class.

findclass will also consult a `.findclass.path` in your home directory, which
is a good place to enumerate platform sources, like the Java sources. For
example:

    /usr/local/java/src/java/lang
    /usr/local/java/src/java/util
    /usr/local/java/src/java/sql
    /usr/local/java/src/java/io
    /usr/local/java/src/java/nio
    /usr/local/java/src/java/text
    /usr/local/java/src/java/math
    /usr/local/java/src/java/net
    /usr/local/java/src/java/awt
    /usr/local/java/src/java/applet
    /usr/local/java/src/java/beans
    /usr/local/java/src/java/rmi
    /usr/local/java/src/java/security
    /usr/local/java/src/javax

You might be wondering why this example enumerates the subdirectories. This is
because findclass returns the first type it can find with the requested name.
So if you want `java.util.List` rather than `java.awt.List` when you ask for
`List`, then you must order your to-be-searched directories accordingly.

[findclass.jar]: https://raw.github.com/samskivert/findclass/master/bin/findclass.jar
[findclass.el]: https://raw.github.com/samskivert/findclass/master/src/main/elisp/findclass.el
