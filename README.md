

# arrg: Flexible argument parsing for R scripts

[R](https://www.r-project.org) is a scripting language. While often used interactively in exploratory data science, or run batch-style to replicate a previous analysis, the language can also be used for shell-like scripting at a command line. This usage is supported by the `Rscript` binary provided with R, and by [the `littler` project](https://github.com/eddelbuettel/littler), but parsing script arguments requires additional effort.

There are several R packages available to provide option and argument parsing, and choosing between them is largely a matter of taste. [`docopt`](http://docopt.org) is cute, and [available for R](https://cran.r-project.org/package=docopt) thanks to Edwin de Jonge, but I find [some of its heuristics strange](https://mastodon.online/@jonclayden/112213538389204350) and that puts me off. Also available on CRAN are [`argparse`](https://CRAN.R-project.org/package=argparse), [`argparser`](https://CRAN.R-project.org/package=argparser), [`batch`](https://CRAN.R-project.org/package=batch), [`defineOptions`](https://cran.r-project.org/package=defineOptions), [`getopt`](https://cran.r-project.org/package=getopt), [`GetoptLong`](https://cran.r-project.org/package=GetoptLong), [`optparse`](https://cran.r-project.org/package=optparse) and [`scribe`](https://cran.r-project.org/package=scribe).

But none of these suited me, because I'm picky, so I wrote my own.

## Installation and status

The `arrg` package is not yet on CRAN, though it should be soon. In the meantime it can be installed using the `remotes` package:


``` r
# install.packages("remotes")
remotes::install_github("jonclayden/arrg")
```

**The package is still in an experimental phase**, so the interface and syntax are likely to change as it is developed. Among features planned but not currently implemented are default patterns, subcommands and automatic help options.

## Creating a parser

The package's key function is also called `arrg`. It takes structured information about how your command is to be used, and creates a parser to extract the necessary information from user arguments, and check them for validity.

Here's an example.


``` r
#! /usr/bin/env Rscript --vanilla

library(arrg)

parser <- arrg("test",
 			   opt("h,help", "Display this usage information and exit"),
 			   opt("n,times", "Run test the specifed number of times", arg="count", default=1L),
 			   opt("t,time", "Print the overall run-time once the test is completed"),
 			   opt("install", "Install the code before testing it"),
 			   patterns=list(pat(options="h!"),
			   			     pat("command", "arg...?", options="nt"),
			     		     pat("path?", options="n,t,install")),
 			   header="Test your code",
 			   footer="Run the test on the code at the specified path (default \".\"), or run a specific command.")
```

There's quite a lot going on here, so let's take it a bit at a time.

The ["shebang"](https://en.wikipedia.org/wiki/Shebang_(Unix)) line starting `#!` allows the script to be run directly from the command line without explicitly passing it through `Rscript` or `r`. It's not required but is a helpful convenience on Unix-like systems.

The first argument to `arrg()`, in this case `"test"`, defines the name of the command. That should generally match the name of the file, but this is an abstract example without a file so it could be anything suitable.

The various calls to `opt()` specify options that the command accepts. When the user calls the command they would need to preface them with one or two hyphens, for short and long versions respectively, but when specifying them this is optional. A short (single-character) or long (word-length) option must be specified, or both, separated by a comma as in `"h,help"`. The second argument gives a description of the option to help the user, which is displayed in the usage output.

Some options take an argument, like the `-n` or `--times` option above. In that case, we specify the name of the argument (`arg="count"`) and, optionally, a default value (`default=1L`). Since the default is of integer mode, any specified value of this argument will be coerced to integer.

Patterns give mutually exclusive ways in which the command may be used, and will be displayed separately in the usage information. Here, we have one pattern that requires the `-h` (or `--help`) option, as the exclamation mark indicates. The second pattern accepts a subcommand and possible arguments (the ellipsis, `...`, indicates one or more values, and the question mark, `?`, that the argument is optional), and accepts the `-n` and `-t` options. The third takes an optional path and accepts the `-n`, `-t` and `--install` options.

This is all made explicit to the user in the usage information, which can be shown using the `show()` method:


``` r
parser$show()
## Test your code
## 
## Usage:
##   test -h
##   test [-n <count>] [-t] <command> [<arg>...]
##   test [-n <count>] [-t] [--install] [<path>]
## 
## Options:
##   -h, --help                    Display this usage information and exit
##   -n <count>, --times=<count>   Run test the specifed number of times
##   -t, --time                    Print the overall run-time once the test is
##                                 completed
##   --install                     Install the code before testing it
## 
## Run the test on the code at the specified path (default "."), or run a specific
## command.
```