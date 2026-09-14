args <- arrg("test",
             opt("h,help", "Display this usage information and exit"),
             opt("n,times", "Run test the specifed number of times", arg="count", default=1L),
             opt("t,time", "Print the overall run-time once the test is completed"),
             opt("install", "Install the code before testing it"),
             patterns=list(pat(.options="h!"),
                           pat("command", "arg...?", .options="nt"),
                           pat("path?", .options="n,t,install")),
             header="Test your code",
             footer="Run the test on the code at the specified path (default \".\"), or run a specific command.")

expect_stdout(args$show(), "usage")

p1 <- args$parse("-h")
p2 <- args$parse(c("-tn3", "--install", "."))       # NB: --install flag is the only thing that marks this as the third pattern not the second
p3 <- args$parse(c("-t", "mycommand", "one", "two"))

expect_true(p1$help)
expect_null(p2$help)
expect_true(p2$time)
expect_equal(p2$times, 3L)
expect_true(p2$install)
expect_equal(p2$path, ".")
expect_true(p3$time)
expect_equal(p3$command, "mycommand")
expect_equal(p3$arg, c("one","two"))

# Specification errors: bad options, syntax errors, too many variable-length arguments
expect_error(arrg("test", opt("h")), "description")
expect_error(arrg("test", opt("h,help,he", "empty")), "too many labels")
expect_error(arrg("test", patterns=list(pat(.options="h"))), "options")
expect_error(arrg("test", patterns=list(pat("command!"))), "Format")
expect_error(arrg("test", patterns=list(pat("source...", "target..."))), "multiple values")

# Usage errors: non-existent options, missing arguments, ambiguity
expect_error(args$parse("-i"), "Unexpected")
expect_error(args$parse("--error"), "Unexpected")
expect_error(args$parse("-n"), "argument")
expect_error(args$parse("--times"), "argument")
expect_error(args$parse("--time=yes"), "argument")
expect_warning(args$parse(c("-n", "-3")), "parameter")
expect_error(args$parse(c("-h", "-t")), "pattern")

# Option labels: leading hyphens and surrounding whitespace are stripped
expect_equal(opt("-h,--help","x")$long, "help")
expect_equal(opt("h, help","x")$short, "h")
expect_equal(opt("h, help","x")$long, "help")
expect_error(opt("h!lp","x"), "alphanumeric")
expect_error(arrg("test", "notanopt"), "opt()")

# Duplicate labels are caught when the parser is created
expect_error(arrg("test", opt("v,verbose","a"), opt("v,vlevel","b")), "Duplicate short")
expect_error(arrg("test", opt("v,verbose","a"), opt("w,verbose","b")), "Duplicate long")

# Defaults keep their own modes, whatever other options are specified
mixed <- arrg("test", opt("h,help","a"), opt("n,times","b",arg="c",default=1L),
              opt("s,scale","c",arg="x",default=2.5), opt("o,out","d",arg="f",default="stdout"),
              patterns=list(pat(.options="hnso")))
defaults <- mixed$parse(character(0))
expect_equal(defaults$help, FALSE)
expect_equal(defaults$times, 1L)
expect_equal(defaults$scale, 2.5)
expect_equal(defaults$out, "stdout")
expect_equal(mixed$parse(c("-n","7"))$times, 7L)

# A default implies an argument, named after the option unless specified
expect_true(opt("o,out","d",default="x")$arg)
expect_equal(opt("o,out","d",default="x")$argname, "out")
expect_equal(opt("n,num","d",default=c(1L,2L))$default, c(1L,2L))
expect_equal(opt("n,num","d",arg="v")$default, NA_character_)

# A parser with no options at all is still usable
bare <- arrg("bare", patterns=list(pat("x...")))
expect_equal(bare$parse("foo")$x, "foo")
expect_stdout(bare$show(), "Usage")
expect_error(bare$parse("-abc"), "Unexpected")

# Unknown short-option clusters are reported whole, not split at "NA"
naish <- arrg("test", opt("install","a"), opt("v","b"), patterns=list(pat("x?",.options="v,install")))
expect_error(naish$parse("-NAv"), "-NAv")

# Whitespace in a pattern's option list, and an empty one
expect_true(arrg("test", opt("h,help","a"), opt("install","b"),
                 patterns=list(pat(.options="h, install")))$parse("--install")$install)
expect_equal(arrg("test", opt("v","a"), patterns=list(pat("x",.options="")))$parse("q")$x, "q")

# Usage output: no <NA> argument names, and patterns of differing wrapped length
expect_stdout(arrg("test", opt("o,out","Output file",default="x"),
                   patterns=list(pat(.options="o")))$show(), "<out>")
wrapped <- arrg("cmd", opt("n,times","d",arg="count"), opt("v","verbose"),
                patterns=list(pat(.options="v"),
                              pat("aaaaaaaaaa","bbbbbbbbbb","cccccccccc","dddddddddd", .options="nv")))
expect_stdout(wrapped$show(width=40), "Usage")

# The "--" terminator ends option parsing, and protects what follows
expect_equal(args$parse(c("--","-tn3"))$command, "-tn3")
expect_false(args$parse(c("--","-tn3"))$time)
expect_equal(args$parse(c("-t","--","-n5"))$command, "-n5")

# Options and positional arguments may be interleaved
expect_equal(args$parse(c("mycommand","-tn3"))$command, "mycommand")
expect_true(args$parse(c("mycommand","-tn3"))$time)
expect_equal(args$parse(c("mycommand","-tn3"))$times, 3L)
expect_equal(args$parse(c("-t","one","-n","3","two"))$command, "one")
expect_equal(args$parse(c("-t","one","-n","3","two"))$arg, "two")

# Short-option clusters, with a value attached or detached
expect_equal(args$parse(c("-tn3","."))$times, 3L)
expect_true(args$parse(c("-tn3","."))$time)
expect_equal(args$parse(c("-n3","."))$times, 3L)
expect_equal(args$parse(c("-n","3","."))$times, 3L)
expect_error(args$parse("-tx"), "-x")      # names the offending letter
expect_error(args$parse("-xyz"), "-xyz")   # not a cluster at all, so named whole

# Long-form labels may be internally hyphenated
hyphenated <- arrg("test", opt("n,dry-run","Do nothing"), opt("o,out","Output",arg="file"),
                   patterns=list(pat("path?", .options="n,o")))
expect_equal(opt("--dry-run","x")$long, "dry-run")
expect_true(hyphenated$parse("--dry-run")[["dry-run"]])
expect_true(hyphenated$parse("-n")[["dry-run"]])
expect_stdout(hyphenated$show(), "--dry-run")
expect_error(opt("dry-","x"), "hyphenated")

# A lone "-" is a positional argument, conventionally standard input
expect_equal(args$parse("-")$command, "-")
expect_equal(hyphenated$parse(c("-o","-"))$out, "-")

# A value attached with "=" may be empty
expect_equal(hyphenated$parse("--out=")$out, "")
expect_equal(hyphenated$parse("--out=x")$out, "x")

# Optional positional arguments that aren't given are absent, not NA
optional <- arrg("test", opt("v","Be verbose"),
                 patterns=list(pat("src","dest?",.options="v"), pat("rest...?")))
expect_null(optional$parse("a")$dest)
expect_equal(optional$parse(c("a","b"))$dest, "b")
expect_null(optional$parse(character(0))$rest)
expect_equal(optional$parse("x")$src, "x")

# Required positional arguments cannot follow optional ones
expect_error(arrg("test", patterns=list(pat("a?","b"))), "cannot follow")
expect_silent(arrg("test", patterns=list(pat("a","b?"))))

# Extra positional arguments are rejected, not silently dropped
expect_error(arrg("test", opt("h,help","Help"),
                  patterns=list(pat(.options="h!")))$parse(c("-h","extra")), "too many")

# An invalid value for an option is reported against that option
typed <- arrg("test", opt("n,times","Count",arg="count",default=1L),
              opt("f,flag","Boolean",default=TRUE), patterns=list(pat(.options="nf")))
expect_error(typed$parse("--times=abc"), "--times")
expect_error(typed$parse("--times=abc"), "integer")
expect_error(typed$parse(c("-n","abc")), "-n")
expect_error(typed$parse("--flag=maybe"), "logical")
expect_equal(typed$parse("--flag=TRUE")$flag, TRUE)

# Failure to match reports why each pattern in turn was rejected
expect_error(args$parse(c("-h","-t")), "do not match any usage pattern")
expect_error(args$parse(c("-h","-t")), "--install")     # each pattern is listed
expect_error(optional$parse("-v"), "is required")       # with its own reason
expect_error(arrg("test")$parse(character(0)), "No usage patterns")

# A positional argument given as a named element takes the name as its
# specification and the value as a default, and is thereby optional
defaulted <- arrg("test", opt("i,install","Install first"),
                  patterns=list(pat(path=".", .options="i"), pat(.options="i")))
expect_equal(defaulted$parse(character(0))$path, ".")
expect_equal(defaulted$parse("/tmp")$path, "/tmp")
expect_equal(defaulted$parse("-i")$path, ".")
expect_false(defaulted$parse(character(0))$install)

# Defaults may be of any mode, and given values are coerced to match
typedArgs <- arrg("test", patterns=list(pat("src", count=1L)))
expect_equal(typedArgs$parse("a")$count, 1L)
expect_equal(typedArgs$parse(c("a","5"))$count, 5L)
expect_true(is.integer(typedArgs$parse(c("a","5"))$count))
expect_equal(typedArgs$parse("a")$src, "a")

# A variadic argument may carry a default too
variadic <- arrg("test", patterns=list(pat(paths...=".")))
expect_equal(variadic$parse(character(0))$paths, ".")
expect_equal(variadic$parse(c("a","b"))$paths, c("a","b"))

# A default makes an argument optional, so it must still come last
expect_error(arrg("test", patterns=list(pat(a=".", "b"))), "cannot follow")
expect_silent(arrg("test", patterns=list(pat("a", b="."))))

# A positional argument of the wrong mode rules out that pattern only, rather
# than failing outright, since another pattern may accept it
either <- arrg("test", patterns=list(pat(count=1L), pat("name")))
expect_equal(either$parse("5")$count, 5L)
expect_equal(either$parse("abc")$name, "abc")
expect_error(arrg("test", patterns=list(pat(count=1L)))$parse("abc"), "not valid for argument")

# Defaults of differing modes in one pattern keep their own modes
mixedArgs <- arrg("test", patterns=list(pat(a="x", b=2L, c=3.5)))
result <- mixedArgs$parse(character(0))
expect_equal(result$a, "x")
expect_equal(result$b, 2L)
expect_equal(result$c, 3.5)

# The pattern options parameter is dotted, so it cannot be confused with a
# positional argument; the old spelling is caught rather than silently taken
# as a defaulted positional argument called "options"
expect_error(pat(options="h!"), 'now called "\\.options"')
expect_equal(attr(pat(.options="h!"), "options"), "h!")

# Usage output adapts to the width available, rather than letting the option
# labels squeeze the description column down to one word per line
usageLines <- function (parser, width) {
    con <- textConnection("out", "w", local=TRUE)
    parser$show(con, width=width)
    close(con)
    out
}

# No line exceeds the requested width, at any width where the labels do fit
for (w in c(40, 50, 60, 70, 80, 100))
    expect_true(all(nchar(usageLines(args, w), "width") <= w))

# A label taking up more than 60% of the width is split across two lines
wide <- arrg("build",
             opt("o,output-directory", "Directory in which to place the built artefacts", arg="directory"),
             opt("v,verbose", "Print more information"),
             patterns=list(pat("target?", .options="ov")))
expect_false(any(usageLines(wide, 80) == "  -o <directory>,"))
expect_true(any(usageLines(wide, 70) == "  -o <directory>,"))

# If there is still no room for a description column, descriptions are placed
# below their labels, and remain wrapped to several words per line
expect_true(any(grepl("^      Directory in which", usageLines(wide, 45))))
# Three description lines in total, rather than the eleven that one word per
# line would produce
expect_equal(sum(grepl("^      \\S", usageLines(wide, 45))), 3L)

# A long command name does not push usage continuation lines off the page
longName <- arrg("run-the-integration-test-suite", opt("n,times","Repeat count",arg="count"),
                 patterns=list(pat("suite","case...?",.options="n")))
expect_true(all(nchar(usageLines(longName, 40), "width") <= 40))

# opt() and pat() are supplied by arrg() itself rather than found in scope, so
# that a script can call arrg::arrg() without attaching the package. Local
# definitions are therefore masked, which is what makes this testable here
mechanism <- local({
    opt <- function (...) stop("the local opt() should not be used")
    pat <- function (...) stop("the local pat() should not be used")
    arrg("test", opt("v","Be verbose"), patterns=list(pat("x", .options="v")))
})
expect_true(mechanism$parse(c("-v","q"))$v)
expect_equal(mechanism$parse(c("-v","q"))$x, "q")

# Everything else in a specification still resolves in the calling scope
localDesc <- "Described in the caller"
expect_stdout(arrg("test", opt("v", localDesc),
                   patterns=list(pat(.options="v")))$show(), localDesc)

# Specifications built ahead of time, and do.call(), continue to work
prebuilt <- list(opt("a,alpha","A"), opt("b,beta","B"))
spliced <- do.call(arrg, c(list("test"), prebuilt, list(patterns=list(pat(.options="ab")))))
expect_true(spliced$parse("-ab")$alpha)
expect_true(spliced$parse("-ab")$beta)

patternList <- list(pat("x", .options="v"))
expect_equal(arrg("test", opt("v","V"), patterns=patternList)$parse(c("-v","q"))$x, "q")

# run() in script mode parses the arguments it is given and calls the body,
# which may take the parsed arguments as a list
ran <- NULL
runner <- arrg("test", opt("v,verbose","Be verbose"),
               opt("n,times","Count",arg="count",default=1L),
               patterns=list(pat(path=".", .options="v,n")))
runner$run(function (args) ran <<- args, args=c("-v","-n3","/tmp"), mode="script", exit=FALSE)
expect_equal(ran$path, "/tmp")
expect_equal(ran$times, 3L)
expect_true(ran$verbose)

# ... or take none, in which case they are bound in the body's environment
runner$run(function () ran <<- list(path=path, times=times, verbose=verbose),
           args="/var", mode="script", exit=FALSE)
expect_equal(ran$path, "/var")
expect_equal(ran$times, 1L)
expect_false(ran$verbose)

# A name the parser could have produced but didn't is bound to NULL, and the
# enclosing scope remains reachable
absent <- arrg("test", patterns=list(pat("src","dest?")))
outerValue <- "visible"
absent$run(function () ran <<- list(dest=is.null(dest), outer=outerValue),
           args="a", mode="script", exit=FALSE)
expect_true(ran$dest)
expect_equal(ran$outer, "visible")

# littler makes a script's arguments available in a top-level "argv" variable
assign("argv", c("-v","/usr"), envir=globalenv())
runner$run(function () ran <<- list(path=path, verbose=verbose), mode="script", exit=FALSE)
rm("argv", envir=globalenv())
expect_equal(ran$path, "/usr")
expect_true(ran$verbose)

# In function mode the body is not run; a function is returned whose formals
# correspond to the positional arguments and options, with their defaults
wrapper <- runner$run(function (args) args, mode="function")
expect_true(is.function(wrapper))
expect_equal(names(formals(wrapper)), c("path","verbose","times"))
expect_equal(formals(wrapper)$times, 1L)
expect_equal(wrapper()$path, ".")
expect_equal(wrapper("/tmp")$path, "/tmp")          # positional, via match.call()
expect_equal(wrapper(times=5L)$times, 5L)
expect_true(wrapper(verbose=TRUE)$verbose)
expect_error(wrapper(times="abc"), "not valid for option")
expect_error(wrapper(nope=1), "unused argument")

# Arguments left at their defaults are not treated as having been given, so
# they do not rule out patterns that don't accept them
twoWay <- arrg("test", opt("h,help","Help"), opt("v","Verbose"),
               patterns=list(pat(.options="v"), pat(.options="h!")))
expect_false(twoWay$run(function (args) args, mode="function")()$v)

# A request for help is answered before the patterns are matched, so it works
# even alongside arguments that are otherwise invalid
expect_stdout(twoWay$run(function () NULL, args="--help", mode="script", exit=FALSE), "Usage")
expect_stdout(twoWay$run(function () NULL, args=c("--help","--bogus"), mode="script", exit=FALSE), "Usage")

# Usage errors are reported on standard error, with a hint
msgs <- capture.output(tryCatch(twoWay$run(function () NULL, args="--bogus",
                                           mode="script", exit=FALSE),
                                error=function (cond) NULL), type="message")
expect_true(any(grepl("^test: Unexpected long-style option", msgs)))
expect_true(any(grepl("Try 'test --help' for more information", msgs)))

expect_error(runner$run("not a function"), "must be a function")

# A block of code in braces may be given in place of a function, and behaves
# as a function of no arguments would
expect_equal(runner$run({ path }, args="/tmp", mode="script", exit=FALSE), "/tmp")
expect_equal(runner$run({ times }, args=c("-n","4","/tmp"), mode="script", exit=FALSE), 4L)

# The block must not be run in the course of working out what it is, so an
# invalid set of arguments leaves it untouched
executed <- FALSE
invisible(capture.output(tryCatch(runner$run({ executed <<- TRUE }, args="--bogus",
                                             mode="script", exit=FALSE),
                                  error=function (cond) NULL), type="message"))
expect_false(executed)

# A block gets an evaluation frame of its own, so return() and on.exit() work
cleaned <- FALSE
expect_equal(runner$run({ on.exit(cleaned <<- TRUE); return(path) },
                        args="/var", mode="script", exit=FALSE), "/var")
expect_true(cleaned)

# In function mode a block yields a wrapper, just as a function does
blockWrapper <- runner$run({ path }, mode="function")
expect_equal(names(formals(blockWrapper)), c("path","verbose","times"))
expect_equal(blockWrapper("/usr"), "/usr")
expect_equal(blockWrapper(), ".")

# Anything that is not a literal block is evaluated and must yield a function,
# so a function referred to by name or extracted from a list still works
namedBody <- function (args) args$path
expect_equal(runner$run(namedBody, args="/tmp", mode="script", exit=FALSE), "/tmp")
bodyList <- list(function (args) args$path)
expect_equal(runner$run(bodyList[[1]], args="/var", mode="script", exit=FALSE), "/var")
expect_error(runner$run("not a function"), "must be a function")
expect_error(runner$run(42), "must be a function")
