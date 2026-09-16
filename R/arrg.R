# Classify an argument: "long" for a long-style option, "short" for a short-style
# option or cluster of them, and "other" for anything else, which includes a
# positional argument, a lone "-" (conventionally standard input) and the "--"
# terminator
tokenType <- function (arg)
{
    if (is.na(arg) || arg == "-" || arg == "--" || !(arg %~% "^-"))
        "other"
    else if (arg %~% "^--")
        "long"
    else
        "short"
}

# Coerce a value given for an option or positional argument to the mode of its
# default, reporting any failure in terms of that option or argument rather
# than leaving R to emit a bare coercion warning. Coercion to logical mode
# fails silently, so the result is tested rather than a warning being caught
coerceValue <- function (value, mode, what)
{
    # Note that the replacement function is called directly, rather than
    # assigning to "value", so that the original is left intact to compare to
    result <- suppressWarnings(`storage.mode<-`(value, mode))
    invalid <- which(is.na(result) & !is.na(value))
    if (length(invalid) > 0)
        stop(es("Value \"#{value[invalid[1]]}\" is not valid for #{what}, which takes a value of type #{mode}"))
    return (result)
}

# The description shown for an option, with its default argument value
# appended where there is one to show. A flag takes no argument, and an option
# whose argument was given no default holds NA, so neither of those qualifies
optDescription <- function (o)
{
    if (!o$arg || all(is.na(o$default)))
        return (o$description)
    
    value <- if (is.character(o$default)) encodeString(o$default, quote="\"") else as.character(o$default)
    return (paste0(o$description, " [default ", paste(value, collapse=", "), "]"))
}

#' Create an argument parser
#' 
#' This function creates an argument parser that handles the specified options
#' and usage patterns. To parse arguments or display usage information, the
#' methods \code{parse} or \code{show} contained in the return value should be
#' called.
#' 
#' Options may be given in long form, as in `--times=3` or `--times 3`, or in
#' short form, as in `-n3` or `-n 3`. Several short-form options may be
#' clustered behind a single hyphen, with any option that takes an argument
#' coming last, as in `-tn3`. Options and positional arguments may be freely
#' interleaved. A `--` argument stops option parsing and is discarded:
#' everything after it is treated as positional, even if it begins with a
#' hyphen. A lone `-` is always positional, by convention referring to
#' standard input.
#' 
#' @param name The name of the command.
#' @param ... Option specifications. See [opt()] for details.
#' @param patterns A list of usage patterns that are valid for the command,
#'   each specifying acceptable options and positional arguments, or a single
#'   such pattern. See [pat()] for details. If none is given, one is generated
#'   that accepts all of the command's options and any number of positional
#'   arguments, which are named `args`.
#' @param help Whether to provide a help option, and a usage pattern for it,
#'   if the command does not specify one of its own. A string may be given
#'   instead of `TRUE`, and is used as the option's description. A generated
#'   option of this kind is listed first, and takes no part in a pattern's
#'   `.options=TRUE`, nor in the function returned by the `run` method.
#' @param header,footer Optional paragraphs of text to be prepended and/or
#'   appended to the usage text produced by the `show` method of the return
#'   value. Typically used to introduce the command or give brief guidance on
#'   usage.
#' @return A list with function elements
#' * `parse(args)`: Parse the character vector of arguments passed in, or by
#'   default, the value of `commandArgs(trailingOnly=TRUE)`.
#' * `show(con, width)`: Print a usage summary, detailing the valid options and
#'   patterns. Text will be printed to the specified connection, default
#'   [stdout()], and wrapped to the width given, which defaults to the value of
#'   the standard `width` option. Any default value for an option's argument
#'   is appended to that option's description.
#' * `run(body, args, mode, help, exit)`: Run the body of a script, given as a
#'   function or a block of code in braces, or return a function that will. `args` overrides the arguments to parse, `mode` the
#'   choice between running (`"script"`) and returning a function
#'   (`"function"`), `help` names the option that requests usage information,
#'   and `exit` controls whether the R session is ended after help is given or
#'   a usage error reported. See Details.
#' 
#' @note The option and pattern specifications given to this function are
#'   evaluated with [opt()] and [pat()] in scope, so a script may call
#'   `arrg::arrg()` without attaching the package using `library()`, and
#'   without namespacing each of those nested calls.
#' 
#' The `run` method provides a script's entry point. Given a function holding
#' the body of the script, it either calls it or returns a function that will,
#' according to how the script was invoked. Run from a command line, by
#' `Rscript` or `littler`, the arguments are parsed, a request for help is
#' answered with the usage summary, a usage error is reported on standard
#' error with a non-zero exit status, and otherwise the body is called. When
#' the script is `source()`d instead, nothing is run: the value is a function
#' whose formal arguments correspond to the parser's options and positional
#' arguments, so that the same script can be driven interactively.
#' 
#' The body may take one argument, in which case it receives the parsed
#' arguments as a list, or none, in which case they are bound in the
#' environment it runs in and may be referred to by name. Note that such
#' bindings mask anything of the same name in the enclosing scope, and that a
#' name the parser could have produced but didn't is bound to `NULL`. A block
#' of code in braces may also be given in place of a function, and is
#' equivalent to a function of no arguments. The braces are required: any
#' other expression is evaluated, and must produce a function, which allows a
#' body to be built by a factory or taken from a variable.
#' 
#' @seealso [opt()], [pat()]
#' 
#' @examples
#'   # A simple parser for a command called "test" with only one option, -h
#'   p <- arrg("test", opt("h", "Print help"), patterns=list(pat(.options="h!")))
#'   
#'   # The same, without attaching the package: opt() and pat() are still
#'   # available within the call itself
#'   p <- arrg::arrg("test", opt("h","Print help"), patterns=list(pat(.options="h!")))
#'   
#'   # The body of a script. When the script is called from a command line
#'   # run() calls this directly; when it is source()d, run() instead returns
#'   # a function, as forced here. The mode is detected automatically by default
#'   greet <- arrg("greet", opt("n,name","Who to greet",default="world"),
#'                 patterns=list(pat(.options="n")))
#'   hello <- greet$run(function () cat("Hello,", name, "\n"), mode="function")
#'   hello()
#'   hello(name="reader")
#'   
#'   # Print out usage information
#'   p$show()
#'   
#'   # Parse the option
#'   p$parse("-h")
#' @author Jon Clayden
#' @export
arrg <- function (name, ..., patterns = list(), help = TRUE, header = NULL, footer = NULL)
{
    # The specifications are evaluated with opt() and pat() in scope, so that
    # the package need not be attached; anything else in them is resolved in
    # the caller's environment, as it would be normally
    scope <- list(opt=opt, pat=pat)
    caller <- parent.frame()
    .opts <- lapply(as.list(substitute(list(...)))[-1], eval, envir=scope, enclos=caller)
    patterns <- eval(substitute(patterns), envir=scope, enclos=caller)
    
    if (!all(vapply(.opts, inherits, logical(1), "arrgOption")))
        stop("Options must be specified using the opt() function")
    
    # A help option is provided unless the command specifies one of its own,
    # and comes first, to match the position of its usage pattern. It is
    # marked as generated, which excludes it from a pattern's ".options=TRUE"
    # and from the function that the run method returns
    if (isTRUE(help) || isFALSE(help))
        description <- "Display this usage information and exit"
    else if (is.character(help) && length(help) == 1L && !is.na(help)) {
        description <- help
        help <- TRUE
    } else
        stop("The help argument must be TRUE, FALSE, or a single string")
    
    .generated <- rep(FALSE, length(.opts))
    if (help && !("h" %in% optField(.opts,"short")) && !("help" %in% optField(.opts,"long"))) {
        .opts <- c(list(opt("h,help", description)), .opts)
        .generated <- c(TRUE, .generated)
    }
    
    .short <- optField(.opts, "short")
    .long <- optField(.opts, "long")
    .names <- optField(.opts, "name")
    
    duplicates <- function (labels) unique(labels[!is.na(labels) & duplicated(labels)])
    if (length(duplicates(.short)) > 0)
        stop("Duplicate short-form option label(s): ", paste(duplicates(.short),collapse=", "))
    if (length(duplicates(.long)) > 0)
        stop("Duplicate long-form option label(s): ", paste(duplicates(.long),collapse=", "))
    
    # A single pattern may be given in place of a list of them
    if (inherits(patterns, "arrgPatternSpec"))
        patterns <- list(patterns)
    if (!all(vapply(patterns, inherits, logical(1), "arrgPatternSpec")))
        stop("Usage patterns must be specified using the pat() function")
    
    # With no pattern given, the command takes all of its own options and any
    # number of positional arguments
    if (length(patterns) == 0L) {
        if ("args" %in% .names)
            stop("A default usage pattern cannot be generated, because an option is named \"args\"")
        patterns <- list(pat("args...?", .options=TRUE))
    }
    if (any(.generated))
        patterns <- c(list(pat(.options="h!")), patterns)
    
    # Options are fixed once the parser is created, so resolve patterns and
    # collect default values up front rather than on every call to parse()
    .pats <- lapply(patterns, resolvePattern, .opts, .generated)
    .defaults <- structure(lapply(.opts, "[[", "default"), names=.names)
    
    # Every name that any pattern could contribute to a parsed result
    .allNames <- unique(c(.names[!.generated], unlist(lapply(.pats, function (p) p$args$name))))
    
    # An option's label, preferring the long form, for use in messages
    .label <- function (i) if (is.na(.long[i])) paste0("-",.short[i]) else paste0("--",.long[i])
    
    # Try each pattern in turn, reporting why every one was rejected if none
    # matches, so that the user can see which they were closest to
    .match <- function (parsed) {
        matches <- lapply(.pats, matchPattern, parsed, .defaults)
        failed <- vapply(matches, inherits, logical(1), "arrgMismatch")
        if (all(failed)) {
            reasons <- vapply(matches, function (m) m$reason, character(1))
            usage <- paste0("  ", name, " ", vapply(.pats, formatPattern, character(1)))
            stop(paste(c("Provided arguments do not match any usage pattern:", paste0(usage, ": ", reasons)), collapse="\n"), call.=FALSE)
        }
        return (matches[[which(!failed)[1]]])
    }
    
    .parse <- function (args = commandArgs(trailingOnly=TRUE)) {
        nargs <- length(args)
        
        # The options given, keyed by option name, the labels the user actually
        # used for them, and the positional arguments, in the order given
        values <- list()
        labels <- list()
        positional <- character(0)
        record <- function (o, label, value) {
            values[[o$name]] <<- value
            labels[[o$name]] <<- label
        }
        
        i <- 1L
        repeat {
            if (i > nargs) break
            type <- tokenType(args[i])
            
            if (args[i] == "--") {
                # An explicit end of options: all that follows is positional
                if (i < nargs)
                    positional <- c(positional, args[(i+1):nargs])
                break
            } else if (type == "long") {
                m <- ore_search("^--([\\w-]+)(=(.*))?$", args[i])
                index <- if (is.null(m)) NA_integer_ else match(m[,1], .long)
                if (is.na(index))
                    stop(es("Unexpected long-style option: #{args[i]}"))
                o <- .opts[[index]]
                label <- paste0("--", o$long)
                if (!is.na(m[,2])) {
                    # A value was attached with "=", and may be empty
                    if (!o$arg)
                        stop(es("Long-style option #{label} does not take an argument"))
                    record(o, label, coerceValue(if (is.na(m[,3])) "" else m[,3], o$mode, paste("option", label)))
                } else if (o$arg) {
                    if (i == nargs)
                        stop(es("Long-style option #{label} requires an argument"))
                    else if (tokenType(args[i+1]) != "other")
                        warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to long-style option #{label}"))
                    record(o, label, coerceValue(args[i+1], o$mode, paste("option", label)))
                    i <- i + 1L
                } else {
                    record(o, label, TRUE)
                }
            } else if (type == "short") {
                # A short-style argument may be a cluster of several options.
                # Each is taken in turn, and if one requires an argument then
                # the remainder of the cluster, if any, provides its value
                cluster <- strsplit(ore_subst("^-", "", args[i]), "")[[1]]
                j <- 1L
                while (j <= length(cluster)) {
                    index <- match(cluster[j], .short)
                    if (is.na(index)) {
                        # Name the whole argument if it isn't a cluster at all
                        label <- if (j == 1L) args[i] else paste0("-", cluster[j])
                        stop(es("Unexpected short-style option: #{label}"))
                    }
                    o <- .opts[[index]]
                    label <- paste0("-", o$short)
                    if (!o$arg) {
                        record(o, label, TRUE)
                        j <- j + 1L
                        next
                    }
                    rest <- paste(cluster[-seq_len(j)], collapse="")
                    if (nzchar(rest))
                        record(o, label, coerceValue(rest, o$mode, paste("option", label)))
                    else if (i == nargs)
                        stop(es("Short-style option #{label} requires an argument"))
                    else {
                        if (tokenType(args[i+1]) != "other")
                            warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to short-style option #{label}"))
                        record(o, label, coerceValue(args[i+1], o$mode, paste("option", label)))
                        i <- i + 1L
                    }
                    break   # The rest of the cluster was the option's value
                }
            } else {
                positional <- c(positional, args[i])
            }
            
            i <- i + 1L
        }
        
        parsed <- list(options=values, labels=labels, args=positional)
        
        return (.match(parsed))
    }
    
    .show <- function (con = stdout(), width = getOption("width")) {
        lines <- character(0)
        
        if (!is.null(header))
            lines <- c(lines, strwrap(header, width), "")
        if (length(.pats) > 0) {
            # Continuation lines are normally aligned under the first pattern
            # element, but that leaves too little room if the command name is
            # long relative to the width available
            nameWidth <- nchar(name, "width")
            exdent <- min(3L + nameWidth, max(4L, width %/% 2L))
            lines <- c(lines, "Usage:", unlist(lapply(.pats, function(p) strwrap(paste(name, formatPattern(p)), width, indent=2L, exdent=exdent))), "")
        }
        if (length(.opts) > 0) {
            # A description column narrower than this isn't worth having
            minDescWidth <- 20L
            
            arg <- optField(.opts, "arg", logical(1))
            argname <- optField(.opts, "argname")
            shortStrings <- ifelse(is.na(.short), NA, paste0("-", .short, ifelse(arg, paste0(" <",argname,">"), "")))
            longStrings <- ifelse(is.na(.long), NA, paste0("--", .long, ifelse(arg, paste0("=<",argname,">"), "")))
            both <- !is.na(shortStrings) & !is.na(longStrings)
            optStrings <- ifelse(both, paste(shortStrings,longStrings,sep=", "), ifelse(is.na(shortStrings), longStrings, shortStrings))
            
            # An option label taking up too much of the line has its short and
            # long forms split across two lines, which narrows the label column
            split <- both & nchar(optStrings,"width") > 0.6 * width
            optLines <- lapply(seq_along(.opts), function (i) {
                if (split[i]) c(paste0(shortStrings[i],","), longStrings[i]) else optStrings[i]
            })
            maxWidth <- max(vapply(optLines, function (l) max(nchar(l,"width")), numeric(1)))
            
            # If the labels are wide enough to squeeze out the description
            # column altogether, descriptions go underneath them instead
            descWidth <- width - maxWidth - 5
            stack <- descWidth < minDescWidth
            
            lines <- c(lines, "Options:")
            for (i in seq_along(.opts)) {
                label <- optLines[[i]]
                descLines <- strwrap(optDescription(.opts[[i]]), max(if (stack) width-6L else descWidth, minDescWidth))
                if (stack)
                    lines <- c(lines, paste0("  ", label), paste0("      ", descLines))
                else {
                    # Any label line but the last stands on its own, with the
                    # description starting alongside the last one
                    last <- length(label)
                    if (last > 1)
                        lines <- c(lines, paste0("  ", label[-last]))
                    lines <- c(lines, paste0("  ", label[last], strrep(" ", maxWidth+3-nchar(label[last],"width")), descLines[1]))
                    if (length(descLines) > 1)
                        lines <- c(lines, paste0(strrep(" ", 5+maxWidth), descLines[-1]))
                }
            }
            lines <- c(lines, "")
        }
        if (!is.null(footer))
            lines <- c(lines, strwrap(footer, width), "")
        
        cat(lines, file=con, sep="\n")
    }
    
    # Build a function whose formals correspond to the positional arguments and
    # options of the parser, and which runs the body when it is called. With
    # subcommands this would become one such function for each of them
    .wrapper <- function (body) {
        argNames <- unique(unlist(lapply(.pats, function (p) p$args$name)))
        if (is.null(argNames))
            argNames <- character(0)
        wrapperNames <- c(argNames, .names[!.generated])
        
        # Every formal is given a default, NULL standing for "not supplied",
        # so that a missing required argument is reported by the pattern
        # matcher rather than by R
        defaults <- vector("list", length(wrapperNames))
        names(defaults) <- wrapperNames
        for (i in seq_along(argNames))
            for (p in .pats)
                if (!is.null(p$defaults[[argNames[i]]]))
                    defaults[[i]] <- p$defaults[[argNames[i]]]
        for (i in seq_along(wrapperNames[-seq_along(argNames)]))
            defaults[[length(argNames)+i]] <- .defaults[[wrapperNames[length(argNames)+i]]]
        
        wrapper <- function () .invoke(body, environment(), match.call())
        if (length(wrapperNames) > 0L)
            formals(wrapper) <- as.pairlist(defaults)
        return (wrapper)
    }
    
    # Reconstruct the same intermediate representation that the command line
    # produces, so that both routes share one set of semantics
    .invoke <- function (body, frame, call) {
        # Only arguments the caller actually gave are passed on, so that the
        # pattern matcher applies defaults itself, exactly as it does for the
        # command line. match.call() names any given positionally
        suppliedNames <- names(as.list(call)[-1])
        supplied <- mget(if (is.null(suppliedNames)) character(0) else suppliedNames, envir=frame)
        supplied <- supplied[!vapply(supplied, is.null, logical(1))]
        
        optNames <- intersect(names(supplied), .names)
        indices <- match(optNames, .names)
        options <- lapply(seq_along(optNames), function (i)
            coerceValue(supplied[[optNames[i]]], .opts[[indices[i]]]$mode, paste("option", .label(indices[i]))))
        names(options) <- optNames
        labels <- structure(as.list(vapply(indices, .label, character(1))), names=optNames)
        
        # Positional arguments are taken in the order each pattern declares
        # them, stopping at the first one that wasn't supplied
        for (p in .pats) {
            positional <- character(0)
            for (n in p$args$name) {
                if (!(n %in% names(supplied))) break
                positional <- c(positional, as.character(supplied[[n]]))
            }
            parsed <- list(options=options, labels=labels, args=positional)
            matched <- matchPattern(p, parsed, .defaults)
            if (!inherits(matched, "arrgMismatch"))
                return (invokeBody(body, matched, .allNames))
        }
        return (.match(list(options=options, labels=labels, args=character(0))))
    }
    
    .run <- function (body, args = NULL, mode = c("auto","script","function"),
                      help = "help", exit = TRUE) {
        # The body is captured unevaluated, so that a block of code may be
        # given as well as a function, and so that identifying which it is
        # never runs it
        body <- resolveBody(substitute(body), parent.frame())
        mode <- match.arg(mode)
        if (mode == "auto")
            mode <- if (beingSourced()) "function" else "script"
        if (mode == "function")
            return (.wrapper(body))
        
        if (is.null(args))
            args <- scriptArgs()
        helpIndex <- if (is.null(help)) NA_integer_ else match(help, .names)
        hint <- if (is.na(helpIndex)) NULL else paste0("Try '", name, " ", .label(helpIndex), "' for more information.")
        
        # A request for help is honoured before the arguments are matched
        # against the patterns, so that it works whatever else was given
        if (!is.na(helpIndex) && helpRequested(args, .opts[[helpIndex]])) {
            .show()
            if (exit) quit("no", status=0L)
            return (invisible(NULL))
        }
        
        # A usage error is the user's mistake rather than the script's, so it
        # is reported briefly on stderr instead of as an R error
        parsed <- tryCatch(.parse(args), error = function (cond) {
            message(name, ": ", conditionMessage(cond))
            if (!is.null(hint)) message(hint)
            if (exit) quit("no", status=1L)
            stop(cond)
        })
        
        return (invisible(invokeBody(body, parsed, .allNames)))
    }
    
    list(parse = .parse, show = .show, run = .run)
}
