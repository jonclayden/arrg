pat <- function (..., options = NULL)
{
    return (structure(list(...), options=options))
}

resolvePattern <- function (spec, optInfo)
{
    argInfo <- list()
    
    if (length(spec) > 0) {
        argMatches <- ore_search("^(\\w+)(\\.\\.\\.)?(\\?)?$", unlist(spec), simplify=FALSE)
        argInfo <- lapply(argMatches, function (m) {
            if (is.null(m))
                stop("Format of positional arguments is invalid")
            else
                data.frame(name=m[,1], format=m[,1], option=FALSE, multiple=!is.na(m[,2]), required=is.na(m[,3]))
        })
        argInfo <- do.call(rbind, argInfo)
        nargs <- nrow(argInfo)
        if (any(argInfo$multiple[-nargs]))
            stop("Only the last positional argument can take multiple values")
    }
    
    if (!is.null(attr(spec, "options"))) {
        opts <- ore_split(",", attr(spec, "options"))
        longMatches <- ore_search("^(\\w+)(!)?$", opts, simplify=FALSE)
        validLongOpts <- longMatches[,,1] %in% optInfo$long
        
        for (i in seq_along(opts)) {
            if (validLongOpts[i]) {
                index <- which(optInfo$long == longMatches[i,,1])
                format <- paste0("--", longMatches[i,,1], ifelse(optInfo$arg[index], paste0("=<",optInfo$argname[index],">"), ""))
                argInfo <- rbind(argInfo, data.frame(name=optInfo$name[index], format=format, option=TRUE, multiple=FALSE, required=!is.na(longMatches[i,,2])))
            } else {
                shortMatches <- ore_search("(\\w)(!)?", opts[i], all=TRUE)
                if (!all(shortMatches[,1] %in% optInfo$short))
                    stop("Pattern uses options not included in the main specification")
                indices <- match(shortMatches[,1], optInfo$short)
                formats <- paste0("-", shortMatches[,1], ifelse(optInfo$arg[indices], paste0(" <",optInfo$argname[indices],">"), ""))
                argInfo <- rbind(argInfo, data.frame(name=optInfo$name[indices], format=formats, option=TRUE, multiple=FALSE, required=!is.na(shortMatches[,2])))
            }
        }
    }
    
    print(argInfo)
    return(argInfo)
}

matchPattern <- function (pattern, parsed)
{
    result <- list()
    
    args <- subset(pattern, !option)
    nargs <- nrow(args)
    if (nargs > 0) {
        npargs <- length(parsed$.args)
        if (sum(args$required) > npargs)
            return (NULL)   # Too few arguments
        if (!any(args$multiple) && npargs > nargs)
            return (NULL)   # Too many arguments
        
        for (i in seq_len(nargs)) {
            if (args$multiple[i] && i <= npargs)
                result[[args$name[i]]] <- parsed$.args[i:npargs]
            else
                result[[args$name[i]]] <- parsed$.args[i]
        }
    }
    
    opts <- subset(pattern, option)
    nopts <- nrow(opts)
    if (nopts > 0) {
        for (i in seq_len(nopts)) {
            name <- opts$name[i]
            if (is.null(parsed[[name]]) && opts$required[i])
                return (NULL)   # Required option missing
            else if (!is.null(parsed[[name]]))
                result[[name]] <- parsed[[name]]
        }
    }
    
    return (result)
}

formatPattern <- function (pattern)
{
    elements <- character(0)
    
    opts <- subset(pattern, option)
    if (nrow(opts) > 0)
        elements <- c(elements, ifelse(opts$required, opts$format, paste0("[",opts$format,"]")))
    
    args <- subset(pattern, !option)
    if (nrow(args) > 0)
        elements <- c(elements, paste0(ifelse(args$required,"<","[<"), args$format, ">", ifelse(args$multiple,"...",""), ifelse(args$required,"","]")))
    
    paste(elements, collapse=" ")
}
