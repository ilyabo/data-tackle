# data-tackle

A Clojure library with a number of useful functions for obtaining and preparing data for visualizations.

## Usage

If you use [Leinigen](https://github.com/technomancy/leiningen), add the following to the dependencies in your project.clj:

      [data-tackle "0.1.0-SNAPSHOT"]

Then, you can use the library functions in your code:

     (ns my-cool-program
       (:require [data-tackle :as dt]))

     (dt/words "Hello, world!")


## License

Copyright © 2013 Ilya Boyandin

Distributed under the Eclipse Public License, the same as Clojure.
