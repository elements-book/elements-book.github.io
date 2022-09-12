# elements-book.github.io

Homepage for projects associated with the book "Elements of ∞-Category Theory"

### Build instructions

This website is generated statically from sources prior to deployment using a tool derived from the [Hakyll](https://jaspervdj.be/hakyll/) website generator. To build or rebuild pages for publication you must first build the generator application, and that process depends on the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/):

* On MacOS, the easiest way to install this is via the [Homebrew]() package manager,
* On Linux, it should be available via the native package manager of your distribution,
* On Windows, you can obtain the installer package for 64-bit Windows from the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) homepage.

Assuming you have the Haskell Tool Stack up and running, simply clone this repository into a directory, open your terminal, and change directory to the root of the newly cloned project. Instructions on building and updating the website follow:

#### Building the generator application

This step is **not** required every time you want to rebuild the website. Generally speaking, it should be executed when you first clone the repository. After that it is only required when the Haskell sources of generator application are updated, which is generally only the case if the overall structure of the website is changed.

Simply execute the following at the command line:

```bash
stack build
```

#### (Re)generating the website

Once the generator application has been built, you may generate the pages of the website for deployment by executing the following command:

```bash
stack exec elements build
```

This command should then be executed each time you change source files, most of which can be found in the `pages/` and `posts/` subdirectories.

If you are editing pages and would like to quickly view how those changes will appear in the generated website you can use the command:

```bash
stack exec elements watch
```

This will start a webserver to view the generated website, which you may access in your web browser at the address [http://127.0.0.1:8000](http://127.0.0.1:8000), then it will continually monitor the source files of the site and automatically regenerate any pages whose sources have changed. You can then view the new version of those pages simply by refreshing your browser.

When you no longer which pages to be updated in this way, you can shut down the watcher and its webserver by typing CTRL-C in the the terminal that is running it.

The site generator application tries to be intelligent in the way that it regenerates the website. Generally speaking it will only regenerate pages whose content will actually be changed by your edits, which can be much faster than regenerating all pages (especially as the website grows). This mechanism can, however, get confused from time to time, with the most common symptom of this confusion being that a page you've edited isn't regenerated on the next `build`. 

If this happens, then you can usually fix things by forcing the regeneration of all pages, using the command:

```bash
stack exec elements rebuild
```
