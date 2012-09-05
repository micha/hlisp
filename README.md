# Development

Install [Leiningen2](https://github.com/technomancy/leiningen)

Install dependencies:

    lein deps

Start a server to serve the app on port 4000:

    script/serve

In another tab, you can start watcher-based ClojureScript compilation:

    script/autobuild

Visit [http://localhost:4000/](http://localhost:4000/) to see the app.

## Running a ClojureScript REPL

### Emacs

* `C-x d` to the `cljs-starter` directory
* `M-x set-variable`, and set the variable `inferior-lisp-program` to `script/cljsrepl`
* `M-x run-lisp` and you should see an inferior-lisp buffer running a ClojureScript REPL.  Then, visit [http://localhost:4000/](http://localhost:4000/).
