

[![CRAN version](https://www.r-pkg.org/badges/version/arrg)](https://cran.r-project.org/package=arrg) [![CI](https://github.com/jonclayden/arrg/actions/workflows/ci.yaml/badge.svg)](https://github.com/jonclayden/arrg/actions/workflows/ci.yaml) [![codecov](https://codecov.io/gh/jonclayden/arrg/graph/badge.svg?token=CZCLmaK8Ty)](https://codecov.io/gh/jonclayden/arrg) [![Dependencies](https://tinyverse.netlify.app/badge/arrg)](https://cran.r-project.org/package=arrg)

# arrg: Flexible argument parsing for R scripts

[R](https://www.r-project.org) is a scripting language. While often used interactively in exploratory data science, or run batch-style to replicate a previous analysis, the language can also be used for shell-like scripting at a command line. This usage is supported by the `Rscript` binary provided with R, and by [the `littler` project](https://github.com/eddelbuettel/littler), but parsing script arguments requires additional effort.

There are several R packages available to provide option and argument parsing, and choosing between them is largely a matter of taste. [`docopt`](http://docopt.org) is cute, and [available for R](https://cran.r-project.org/package=docopt) thanks to Edwin de Jonge, but I find [some of its heuristics strange](https://mastodon.online/@jonclayden/112213538389204350) and that puts me off. Also available on CRAN are [`argparse`](https://CRAN.R-project.org/package=argparse), [`argparser`](https://CRAN.R-project.org/package=argparser), [`batch`](https://CRAN.R-project.org/package=batch), [`defineOptions`](https://cran.r-project.org/package=defineOptions), [`getopt`](https://cran.r-project.org/package=getopt), [`GetoptLong`](https://cran.r-project.org/package=GetoptLong), [`optigrab`](https://cran.r-project.org/package=optigrab), [`optparse`](https://cran.r-project.org/package=optparse) and [`scribe`](https://cran.r-project.org/package=scribe), so there's no shortage of options.

But none of these suited me, [because I'm picky](#why-arrg), so I wrote my own.

## Installation and status

The latest release version of the `arrg` package is available [on CRAN](https://cran.r-project.org/package=arrg). The development version can be installed using the `remotes` package if required:


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
    patterns=list(
        pat(options="h!"),
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

This is all made explicit to the user in the usage information, which can be shown using the `show()` method. This is also where the `header` and `footer` arguments above come in, as they're shown before and after the usage and option summary:


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
##   -h, --help                    Display this usage information and
##                                 exit
##   -n <count>, --times=<count>   Run test the specifed number of times
##   -t, --time                    Print the overall run-time once the
##                                 test is completed
##   --install                     Install the code before testing it
## 
## Run the test on the code at the specified path (default "."), or run
## a specific command.
```

Note that the text blocks are wrapped neatly, each pattern is shown separately, optional arguments are surrounded by square brackets, argument names are used in appropriate places, and so on.

## Parsing arguments

The `parse()` method then does the actual parse. By default it takes script arguments from the result of `commandArgs(trailingOnly=TRUE)`, but arguments can also be specified explicitly. (We would use `argv` for `littler`.)


``` r
parser$parse("-h")
## $help
## [1] TRUE
```

The result is a list with the valid arguments included, keyed by their argument names, long option labels or short option labels, in that order of preference. Note that `-h` is a flag, so its value is a Boolean (`TRUE`/`FALSE`) value. In practice this form would prompt us to `show()` the usage information above. A more interesting example could be


``` r
parser$parse(c("-tn3", "--install", "."))
## $path
## [1] "."
## 
## $times
## [1] 3
## 
## $time
## [1] TRUE
## 
## $install
## [1] TRUE
```

Here, we're using the third pattern, which `arrg` distinguishes from the second by the presence of the `--install` option, which the second pattern doesn't accept. Note that `-tn3` combines the `-t` option with the `-n` option and its argument, which is a common space-saving syntax that `arrg` supports; and that the argument to the `-n` (times) option gets converted to integer mode automatically to match the default value.

The script can now use the parsed argument list to implement its core functionality.

## Why `arrg`?

OK, so why might you want to use `arrg` in preference to one of the many alternatives for R? As I stated at the outset, this is largely a question of taste, but here are my reasons.

For a fairly basic piece of functionality like this, portability is important to me, and that means that I want dependencies to be minimal, especially outside the R ecosystem. That excludes the `argparse` package, which requires Python, and `GetoptLong`, which requires Perl.

Package `batch` doesn't use Unix-style options, but rather an argument list of alternating variable names and values which are interpreted as R expressions. It's an neat and simple solution, but a little verbose to use in my opinion, especially for simple on/off flags. `getopt` is very bare-bones, and is essentially subsumed by `optparse`. I couldn't get `defineOptions` to work as expected using the documentation.

That still leaves various alternatives. `docopt` is a solid option if its heuristics work for you. Indeed, it parses `arrg`'s help string correctly for the test case above, and is the only other package that seems to handle multiple usage patterns and short-option clusters like `-tn3`.


``` r
# Write the usage help output to a character vector
parser$show(textConnection("help", "w"))

docopt::docopt(paste(help,collapse="\n"), c("-tn3", "--install", "."))
## List of 16
##  $ --help   : logi FALSE
##  $ --times  : chr "3"
##  $ --time   : logi TRUE
##  $ --install: logi TRUE
##  $ <command>: NULL
##  $ <arg>    : list()
##  $ <path>   : chr "."
##  $ is       : logi FALSE
##  $ completed: logi FALSE
##  $ help     : logi FALSE
##  $ times    : chr "3"
##  $ time     : logi TRUE
##  $ install  : logi TRUE
##  $ command  : NULL
##  $ arg      : list()
##  $ path     : chr "."
## NULL
```

`optigrab` doesn't use a parser object or up-front interface specification, but just searches for each option on demand. As a result, the requested options must already be in scope when its functions are called. It generates basic usage for options it has seen when `opt_help()` is called.


``` r
library(optigrab)
## optigrab-0.9.2.1 (2019-01-05) - Copyright © 2019 Decision Patterns
```

``` r

opts <- c("-t", "-n", "3", "--install", ".")

list(help=opt_get(c("h","help"), FALSE, description="Display this usage information and exit", opts=opts),
    times=opt_get(c("n","times"), 1L, description="Run test the specifed number of times", opts=opts),
    time=opt_get(c("t","time"), FALSE, description="Print the overall run-time once the test is completed", opts=opts),
    install=opt_get("install", FALSE, description="Install the code before testing it", opts=opts),
    opt_get_verb(opts=opts))
## $help
## [1] FALSE
## 
## $times
## [1] 3
## 
## $time
## [1] TRUE
## 
## $install
## [1] TRUE
## 
## [[5]]
## [1] "."
```

``` r

# This will quit a non-interative R session
# opt_help(opts="--help")
```

The `optparse` package works similarly to `arrg`, although it has quite basic support for positional arguments and creates only a simple usage block by default.


``` r
library(optparse)

op.parser <- OptionParser(prog="test",
    add_help_option=FALSE,
    description="Test your code",
    epilogue="Run the test on the code at the specified path (default \".\"), or run a specific command.") |>
    add_option(c("-h","--help"), "store_true", help="Display this usage information and exit") |>
    add_option(c("-n","--times"), "store", default=1L, help="Run test the specifed number of times") |>
    add_option(c("-t","--time"), "store_true", help="Print the overall run-time once the test is completed") |>
    add_option("--install", "store_true", help="Install the code before testing it")

print_help(op.parser)
## Usage: test [options]
## Test your code
## 
## Options:
## 	-h, --help
## 		Display this usage information and exit
## 
## 	-n TIMES, --times=TIMES
## 		Run test the specifed number of times
## 
## 	-t, --time
## 		Print the overall run-time once the test is completed
## 
## 	--install
## 		Install the code before testing it
## 
## Run the test on the code at the specified path (default "."), or run a specific command.
```

``` r
parse_args(op.parser, "-h", print_help_and_exit=FALSE, positional_arguments=TRUE)
## $options
## $options$help
## [1] TRUE
## 
## $options$times
## [1] 1
## 
## 
## $args
## character(0)
```

``` r
parse_args(op.parser, c("-t", "-n", "3", "--install", "."), print_help_and_exit=FALSE, positional_arguments=TRUE)
## $options
## $options$times
## [1] 3
## 
## $options$time
## [1] TRUE
## 
## $options$install
## [1] TRUE
## 
## 
## $args
## [1] "."
```

And finally, `scribe` generates nice help output, but I couldn't figure out how to fully suppress its default `--help` and `--version` options:


``` r
library(scribe)

s.parser <- command_args(c("-t", "-n", "3", "--install", "."), include=NA_character_)
s.parser$add_description("Test your code")

s.parser$add_argument("-h", "--help", action="flag", options=list(no=FALSE), info="Display this usage information and exit")
s.parser$add_argument("-n", "--times", n=1L, default=1L, info="Run test the specifed number of times")
s.parser$add_argument("-t", "--time", action="flag", options=list(no=FALSE), info="Print the overall run-time once the test is completed")
s.parser$add_argument("--install", action="flag", options=list(no=FALSE), info="Install the code before testing it")
s.parser$add_argument("command", n=1L, info="Command to run or path to code")

s.parser$help()
## {scribe} command_args
## 
## file : {path}
## 
## DESCRIPTION
##   Test your code
## 
## USAGE
##   {command} [--help | --version]
##   {command} [-h, --help] [-n, --times [ARG]] [-t, --time] [--install] [command [ARG]] 
## 
## ARGUMENTS
##   -h, --help        : Display this usage information and exit              
##   -n, --times [ARG] : Run test the specifed number of times                
##   -t, --time        : Print the overall run-time once the test is completed
##   --install         : Install the code before testing it                   
##   command [ARG]     : Command to run or path to code
```

``` r
s.parser$parse()
## $help
## [1] FALSE
## 
## $times
## [1] 3
## 
## $time
## [1] TRUE
## 
## $install
## [1] TRUE
## 
## $command
## [1] "."
```
