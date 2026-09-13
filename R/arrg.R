expandArgs <- function (args, validShort)
{
    validShort <- validShort[!is.na(validShort)]
    if (length(validShort) == 0L)
        return (args)
    
    regex <- ore("^-((", paste(validShort,collapse="|"), ")+)(.*)$")
    result <- character(0)
    for (i in seq_along(args)) {
        if (args[i] %~% regex) {
            m <- ore_lastmatch()
            result <- c(result, paste0("-",unlist(strsplit(m[,1], ""))))
            if (!is.na(m[,3]))
                result <- c(result, m[,3])
        } else {
            result <- c(result, args[i])
        }
    }
    return (result)
}

#' Create an argument parser
#' 
#' This function creates an argument parser that handles the specified options
#' and usage patterns. To parse arguments or display usage information, the
#' methods \code{parse} or \code{show} contained in the return value should be
#' called.
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
        args <- expandArgs(args, .short)
        flags <- as.integer(ore_switch(args, "^--"="2", "^-"="1", "0"))
        nargs <- length(args)
        
        i <- 1
        result <- list()
        repeat {
            if (i > nargs) break
            else if (args[i] == "--") {
                i <- i + 1
                break
            } else if (flags[i] == 2L) {
                m <- ore_search("^--(\\w+)(=(.*))?$", args[i])
                index <- if (is.null(m)) NA_integer_ else match(m[,1], .long)
                if (is.na(index))
                    stop(es("Unexpected long-style option: #{args[i]}"))
                o <- .opts[[index]]
                if (!is.na(m[,3])) {
                    if (!o$arg)
                        stop(es("Long-style option --#{o$long} does not take an argument"))
                    result[[o$name]] <- m[,3] %as% o$mode
                } else if (o$arg) {
                    if (i == nargs)
                        stop(es("Long-style option --#{o$long} requires an argument"))
                    else if (flags[i+1] > 0L)
                        warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to long-style option --#{o$long}"))
                    result[[o$name]] <- args[i+1] %as% o$mode
                    i <- i + 1
                } else {
                    result[[o$name]] <- TRUE
                }
            } else if (flags[i] == 1L) {
                index <- match(ore_subst("^-","",args[i]), .short)
                if (is.na(index))
                    stop(es("Unexpected short-style option: #{args[i]}"))
                o <- .opts[[index]]
                if (o$arg) {
                    if (i == nargs)
                        stop(es("Short-style option -#{o$short} requires an argument"))
                    else if (flags[i+1] > 0L)
                        warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to short-style option -#{o$short}"))
                    result[[o$name]] <- args[i+1] %as% o$mode
                    i <- i + 1
                } else {
                    result[[o$name]] <- TRUE
                }
            } else break
            i <- i + 1
        }
        
        if (nargs >= i)
            result[[".args"]] <- args[i:nargs]
        else
            result[[".args"]] <- character(0)
        
        patternMatches <- lapply(.pats, matchPattern, result, .defaults)
        validPatterns <- !vapply(patternMatches, is.null, logical(1))
        if (!any(validPatterns))
            stop("Provided arguments do not match any usage pattern")
        
        return (patternMatches[[which(validPatterns)[1]]])
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
