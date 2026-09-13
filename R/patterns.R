#' Specify a usage pattern
#' 
#' This function is used to specify a valid usage pattern for the command,
#' which may be one of a number of mutually exclusive patterns available. Its
#' return value is generally passed to [arrg()].
#' 
#' When parsing arguments, patterns are tried in the order specified, and the
#' first valid pattern will be chosen. A pattern will be considered a valid
#' match if all required options and positional arguments have been specified,
#' and no unexpected options are included.
#' 
#' @param ... Character strings naming positional arguments, if any are valid.
#'   Positional arguments are required by default; if not required they should
#'   be followed by a question mark. The final positional argument (only) may
#'   take multiple values, in which case it should contain an ellipsis (...),
#'   before the question mark if the argument is also optional.
#' @param options A string naming the long or short labels of options that can
#'   be specified with this pattern, comma-separated. Short form options may be
#'   given in one letter cluster for convenience. Options are only required if
#'   followed by an exclamation mark.
#' @return A list capturing the positional arguments, with options in an
#'   attribute. This will not usually be used directly, but passed to [arrg()].
#' @seealso [arrg()]
#' 
#' @examples
#'   # A pattern with no positional arguments, but requiring the -h flag
#'   pat(options="h!")
#'   
#'   # A pattern that takes a command and variable number of arguments, and
#'   # accepts the -n and -t options (note the latter are specified in cluster
#'   # form, but "n,t" is also valid) 
#'   pat("command", "arg...?", options="nt")
#' 
#' @author Jon Clayden
#' @export
pat <- function (..., options = NULL)
{
    return (structure(list(...), options=options))
}

resolvePattern <- function (spec, opts)
{
    optShort <- optField(opts, "short")
    optLong <- optField(opts, "long")
    optName <- optField(opts, "name")
    optArg <- optField(opts, "arg", logical(1))
    optArgname <- optField(opts, "argname")
    
    argInfo <- list()
    
    if (length(spec) > 0) {
        argMatches <- ore_search("^(\\w+)(\\.\\.\\.)?(\\?)?$", unlist(spec), simplify=FALSE)
        argInfo <- lapply(argMatches, function (m) {
            if (is.null(m))
                stop("Format of positional arguments is invalid")
            else
                data.frame(name=m[,1], format=m[,1], option=FALSE, multiple=!is.na(m[,2]), required=is.na(m[,3]), stringsAsFactors=FALSE)
        })
        argInfo <- do.call(rbind, argInfo)
        nargs <- nrow(argInfo)
        if (any(argInfo$multiple[-nargs]))
            stop("Only the last positional argument can take multiple values")
    }
    
    if (!is.null(attr(spec, "options"))) {
        labels <- trimws(unlist(ore_split(",", attr(spec, "options"))))
        labels <- labels[nzchar(labels)]
        
        for (label in labels) {
            longMatch <- ore_search("^(\\w+)(!)?$", label)
            index <- if (is.null(longMatch)) NA_integer_ else match(longMatch[,1], optLong)
            if (!is.na(index)) {
                format <- paste0("--", optLong[index], ifelse(optArg[index], paste0("=<",optArgname[index],">"), ""))
                required <- !is.na(longMatch[,2])
            } else {
                # Not a known long-form label, so treat it as a cluster of
                # short-form ones, each optionally followed by an exclamation
                if (!(label %~% "^(\\w!?)+$"))
                    stop("Invalid option specification in pattern: ", label)
                shortMatches <- ore_search("(\\w)(!)?", label, all=TRUE)
                if (!all(shortMatches[,1] %in% optShort))
                    stop("Pattern uses options not included in the main specification")
                index <- match(shortMatches[,1], optShort)
                format <- paste0("-", optShort[index], ifelse(optArg[index], paste0(" <",optArgname[index],">"), ""))
                required <- !is.na(shortMatches[,2])
            }
            argInfo <- rbind(argInfo, data.frame(name=optName[index], format=format, option=TRUE, multiple=FALSE, required=required, stringsAsFactors=FALSE))
        }
    }
    
    return (argInfo)
}

matchPattern <- function (pattern, parsed, defaults)
{
    result <- list()
    
    parsedOptionNames <- setdiff(names(parsed), ".args")
    if (!all(parsedOptionNames %in% pattern$name))
        return (NULL)   # Unexpected option
    
    args <- subset(pattern, !pattern$option)
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
    
    opts <- subset(pattern, pattern$option)
    nopts <- nrow(opts)
    if (nopts > 0) {
        for (i in seq_len(nopts)) {
            name <- opts$name[i]
            if (opts$required[i] && is.null(parsed[[name]]))
                return (NULL)   # Required option missing
            else if (!is.null(parsed[[name]]))
                result[[name]] <- parsed[[name]]
            else if (!is.null(defaults[[name]]))
                result[[name]] <- defaults[[name]]
        }
    }
    
    return (result)
}

formatPattern <- function (pattern)
{
    elements <- character(0)
    
    opts <- subset(pattern, pattern$option)
    if (nrow(opts) > 0)
        elements <- c(elements, ifelse(opts$required, opts$format, paste0("[",opts$format,"]")))
    
    args <- subset(pattern, !pattern$option)
    if (nrow(args) > 0)
        elements <- c(elements, paste0(ifelse(args$required,"<","[<"), args$format, ">", ifelse(args$multiple,"...",""), ifelse(args$required,"","]")))
    
    paste(elements, collapse=" ")
}
