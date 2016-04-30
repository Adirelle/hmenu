
hmenu
=====

`hmenu` is a graphical menu to search and launch applications, inspired by softwares like [Gnome Do](http://do.cooperteam.net), [Launchy](https://sourceforge.net/projects/launchy) and [rofi](https://davedavenport.github.io/rofi).

I started this project to train myself with Haskell. However, having a practical goal helps working with several aspects of Haskell.

Features
--------

* Graphical user interface built with GTK-3.
* Scan PATH for executables.
* Scan `.desktop` files from XDG directories, following the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) and the [Desktop Entry Specification](https://specifications.freedesktop.org/desktop-entry-spec/latest/index.html).
* Display the application with names, comments and icons.
* (Naive) full-text search.

Installation
------------

Right now, only the sources are available, so you get to download the sources and build it with GHC and [stack](http://docs.haskellstack.org/en/stable/README).

    stack setup
    stack install

TODO
----

* Actually launch the application.
* Tests and CI.
* Maybe: support "DBus-activatable" desktop entries.

Usage
-----

Simply type:

    hmenu

Hitting Escape clears the input field or closes the menu if it is already empty.

License
-------

Please see the LICENSE file.
