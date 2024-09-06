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

arrg <- function (name, ..., patterns = list(), header = NULL, footer = NULL)
{
    .opts <- rbind(...)
    .pats <- lapply(patterns, resolvePattern, .opts)
    print(.opts)
    list(parse = function (args = commandArgs(trailingOnly=TRUE)) {
        args <- expandArgs(args, .opts$short)
        flags <- as.integer(ore_switch(args, "^--"="2", "^-"="1", "0"))
        nargs <- length(args)
        
        i <- 1
        result <- structure(mapply("%as%", .opts$default, .opts$mode, SIMPLIFY=FALSE), names=.opts$name)
        repeat {
            if (i > nargs) break
            else if (args[i] == "--") {
                i <- i + 1
                break
            } else if (flags[i] == 2L) {
                match <- ore_search("^--(\\w+)(=(.*))?$", args[i])
                opt <- subset(.opts, long==match[,1])
                if (nrow(opt) != 1L)
                    error(es("Unexpected long-style option: #{args[i]}"))
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
                opt <- subset(.opts, short==ore_subst("^-","",args[i]))
                if (nrow(opt) != 1L)
                    error(es("Unexpected short-style option: #{args[i]}"))
                name <- ifelse(is.na(opt$long), opt$short, opt$long)
                if (opt$arg) {
                    if (i == nargs)
                        stop(es("Short-style option --#{opt$short} requires an argument"))
                    else if (flags[i+1] > 0L)
                        warning(es("Flag-like argument #{args[i+1]} will be taken as a parameter to short-style option --#{opt$short}"))
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
        
        patternMatches <- lapply(.pats, matchPattern, result)
        validPatterns <- !sapply(patternMatches, is.null)
        print(validPatterns)
        if (!any(validPatterns))
            stop("Provided arguments do not match any usage pattern")
        
        return (patternMatches[[which(validPatterns)[1]]])
    }, show = function (width = getOption("width")) {
        wrap(header, width)
        
        wrap(footer, width)
    })
}
