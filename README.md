# `frontit`

**Frontit is a work in progress.**

Frontit is a tiny server designed to be a read-only front-end used with a [Gitit wiki](https://hackage.haskell.org/package/gitit-0.12.2). It understands the same notion of files and directories as Gitit, but simply serves them directly. The goal is to use Gitit to manage the content of a web site (either locally or remotely), but then display that web site for general viewing through a different system entirely.


## Current Usage

```
frontit
  -p port  --port=port      The port to serve on
  -d path  --data=path      The location of the data directory
  -c path  --config=path    The location of the gitit configuration
  -t path  --template=path  The location of the desired HTML template
```

`frontit` will run a basic HTTP server that takes all requests and attempts to find a corresponding `.page` file in the data directory (which by default is the current working directory), reads the YAML front matter to find out whether the page has `public = yes` set somewhere, and if so, renders it using the provided template (which defaults to a very barebones HTML file) and serves it on the provided port.

The `gitit` configuration can be used to force `frontit` to more closely adhere to an existing `gitit` instance: for example, by default `gitit` usually serves `Front Page.page` as the first page of the wiki, but this can be changed in the configuration; `frontit` will follow the same convention. Similarly, the `gitit` configuration can specify a default language for files that do not specify one: this is usually Markdown, but a `gitit` user might prefer all files to be in RST. Pointing `frontit` at the same `gitit` configuration can allow it to follow suit.
