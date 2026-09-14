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
#'   each specifying acceptable options and positional arguments. See [pat()]
#'   for details.
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
#'   the standard `width` option.
#' @seealso [opt()], [pat()]
#' 
#' @examples
#'   # A simple parser for a command called "test" with only one option, -h
#'   p <- arrg("test", opt("h", "Print help"), patterns=list(pat(options="h!")))
#'   
#'   # Print out usage information
#'   p$show()
#'   
#'   # Parse the option
#'   p$parse("-h")
#' @author Jon Clayden
#' @export
arrg <- function (name, ..., patterns = list(), header = NULL, footer = NULL)
{
    .opts <- list(...)
    if (!all(vapply(.opts, inherits, logical(1), "arrgOption")))
        stop("Options must be specified using the opt() function")
    
    .short <- optField(.opts, "short")
    .long <- optField(.opts, "long")
    .names <- optField(.opts, "name")
    
    duplicates <- function (labels) unique(labels[!is.na(labels) & duplicated(labels)])
    if (length(duplicates(.short)) > 0)
        stop("Duplicate short-form option label(s): ", paste(duplicates(.short),collapse=", "))
    if (length(duplicates(.long)) > 0)
        stop("Duplicate long-form option label(s): ", paste(duplicates(.long),collapse=", "))
    
    # Options are fixed once the parser is created, so resolve patterns and
    # collect default values up front rather than on every call to parse()
    .pats <- lapply(patterns, resolvePattern, .opts)
    .defaults <- structure(lapply(.opts, "[[", "default"), names=.names)
    
    list(parse = function (args = commandArgs(trailingOnly=TRUE)) {
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
                    record(o, label, coerceValue(if (is.na(m[,3])) "" else m[,3], o$mode, label))
                } else if (o$arg) {
                    if (i == nargs)
                        stop(es("Long-style option #{label} requires an argument"))
                    else if (tokenType(args[i+1]) != "other")
                        warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to long-style option #{label}"))
                    record(o, label, coerceValue(args[i+1], o$mode, label))
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
                        record(o, label, coerceValue(rest, o$mode, label))
                    else if (i == nargs)
                        stop(es("Short-style option #{label} requires an argument"))
                    else {
                        if (tokenType(args[i+1]) != "other")
                            warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to short-style option #{label}"))
                        record(o, label, coerceValue(args[i+1], o$mode, label))
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
        
        if (length(.pats) == 0)
            stop("No usage patterns have been specified for this command", call.=FALSE)
        
        matches <- lapply(.pats, matchPattern, parsed, .defaults)
        failed <- vapply(matches, inherits, logical(1), "arrgMismatch")
        if (all(failed)) {
            # Report why each pattern in turn was rejected, so that the user
            # can see which one they were closest to matching
            reasons <- vapply(matches, function (m) m$reason, character(1))
            usage <- paste0("  ", name, " ", vapply(.pats, formatPattern, character(1)))
            stop(paste(c("Provided arguments do not match any usage pattern:", paste0(usage, ": ", reasons)), collapse="\n"), call.=FALSE)
        }
        
        return (matches[[which(!failed)[1]]])
    }, show = function (con = stdout(), width = getOption("width")) {
        lines <- character(0)
        
        if (!is.null(header))
            lines <- c(lines, strwrap(header, width), "")
        if (length(.pats) > 0) {
            nameWidth <- nchar(name, "width")
            lines <- c(lines, "Usage:", unlist(lapply(.pats, function(p) strwrap(paste(name, formatPattern(p)), width, indent=2L, exdent=3L+nameWidth))), "")
        }
        if (length(.opts) > 0) {
            arg <- optField(.opts, "arg", logical(1))
            argname <- optField(.opts, "argname")
            shortStrings <- ifelse(is.na(.short), NA, paste0("-", .short, ifelse(arg, paste0(" <",argname,">"), "")))
            longStrings <- ifelse(is.na(.long), NA, paste0("--", .long, ifelse(arg, paste0("=<",argname,">"), "")))
            both <- !is.na(shortStrings) & !is.na(longStrings)
            optStrings <- ifelse(both, paste(shortStrings,longStrings,sep=", "), ifelse(is.na(shortStrings), longStrings, shortStrings))
            optWidths <- nchar(optStrings, "width")
            maxWidth <- max(optWidths)
            
            lines <- c(lines, "Options:")
            for (i in seq_along(.opts)) {
                descLines <- strwrap(.opts[[i]]$description, width-maxWidth-5)
                lines <- c(lines, paste0("  ", optStrings[i], strrep(" ", maxWidth+3-optWidths[i]), descLines[1]))
                if (length(descLines) > 1)
                    lines <- c(lines, paste0(strrep(" ", 5+maxWidth), descLines[-1]))
            }
            lines <- c(lines, "")
        }
        if (!is.null(footer))
            lines <- c(lines, strwrap(footer, width), "")
        
        cat(lines, file=con, sep="\n")
    })
}
