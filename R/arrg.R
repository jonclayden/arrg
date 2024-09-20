expandArgs <- function (args, validShort)
{
    result <- character(0)
    for (i in seq_along(args)) {
        if (args[i] %~% ore("^-((", paste(validShort,collapse="|"), ")+)(.*)$")) {
            result <- c(result, paste0("-",unlist(strsplit(ore_lastmatch()[,1], ""))))
            if (!is.na(ore_lastmatch()[,3]))
                result <- c(result, ore_lastmatch()[,3])
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
    .opts <- rbind(...)
    .pats <- lapply(patterns, resolvePattern, .opts)
    list(parse = function (args = commandArgs(trailingOnly=TRUE)) {
        args <- expandArgs(args, .opts$short)
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
                match <- ore_search("^--(\\w+)(=(.*))?$", args[i])
                opt <- subset(.opts, .opts$long==match[,1])
                if (nrow(opt) != 1L)
                    stop(es("Unexpected long-style option: #{args[i]}"))
                if (!is.na(match[,3])) {
                    if (!opt$arg)
                        stop(es("Long-style option --#{opt$long} does not take an argument"))
                    result[[opt$long]] <- match[,3] %as% opt$mode
                } else if (opt$arg) {
                    if (i == nargs)
                        stop(es("Long-style option --#{opt$long} requires an argument"))
                    else if (flags[i+1] > 0L)
                        warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to long-style option --#{opt$long}"))
                    result[[opt$long]] <- args[i+1] %as% opt$mode
                    i <- i + 1
                } else {
                    result[[opt$long]] <- TRUE
                }
            } else if (flags[i] == 1L) {
                opt <- subset(.opts, .opts$short==ore_subst("^-","",args[i]))
                if (nrow(opt) != 1L)
                    stop(es("Unexpected short-style option: #{args[i]}"))
                name <- ifelse(is.na(opt$long), opt$short, opt$long)
                if (opt$arg) {
                    if (i == nargs)
                        stop(es("Short-style option -#{opt$short} requires an argument"))
                    else if (flags[i+1] > 0L)
                        warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to short-style option -#{opt$short}"))
                    result[[name]] <- args[i+1] %as% opt$mode
                    i <- i + 1
                } else {
                    result[[name]] <- TRUE
                }
            } else break
            i <- i + 1
        }
        
        if (nargs >= i)
            result[[".args"]] <- args[i:nargs]
        else
            result[[".args"]] <- character(0)
        
        defaults <- structure(mapply("%as%", .opts$default, .opts$mode, SIMPLIFY=FALSE), names=.opts$name)
        patternMatches <- lapply(.pats, matchPattern, result, defaults)
        validPatterns <- !sapply(patternMatches, is.null)
        if (!any(validPatterns))
            stop("Provided arguments do not match any usage pattern")
        
        return (patternMatches[[which(validPatterns)[1]]])
    }, show = function (con = stdout(), width = getOption("width")) {
        lines <- character(0)
        
        if (!is.null(header))
            lines <- c(lines, strwrap(header, width), "")
        if (length(.pats) > 0) {
            nameWidth <- nchar(name, "width")
            lines <- c(lines, "Usage:", sapply(.pats, function(p) strwrap(paste(name, formatPattern(p)), width, indent=2L, exdent=3L+nameWidth)), "")
        }
        if (nrow(.opts) > 0) {
            shortStrings <- ifelse(is.na(.opts$short), NA, paste0("-", .opts$short, ifelse(.opts$arg, paste0(" <",.opts$argname,">"), "")))
            longStrings <- ifelse(is.na(.opts$long), NA, paste0("--", .opts$long, ifelse(.opts$arg, paste0("=<",.opts$argname,">"), "")))
            both <- !is.na(shortStrings) & !is.na(longStrings)
            optStrings <- ifelse(both, paste(shortStrings,longStrings,sep=", "), ifelse(is.na(shortStrings), longStrings, shortStrings))
            optWidths <- nchar(optStrings, "width")
            maxWidth <- max(optWidths)
            
            lines <- c(lines, "Options:")
            for (i in seq_len(nrow(.opts))) {
                descLines <- strwrap(.opts$description[i], width-maxWidth-5)
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
