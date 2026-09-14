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
#'   be followed by a question mark. Optional arguments must come after all
#'   required ones. The final positional argument (only) may take multiple
#'   values, in which case it should contain an ellipsis (...), before the
#'   question mark if the argument is also optional.
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

# An indication that a pattern did not match, with the reason why, as distinct
# from a list of matched arguments
mismatch <- function (reason)
{
    return (structure(list(reason=reason), class="arrgMismatch"))
}

resolvePattern <- function (spec, opts)
{
    optShort <- optField(opts, "short")
    optLong <- optField(opts, "long")
    optName <- optField(opts, "name")
    optArg <- optField(opts, "arg", logical(1))
    optArgname <- optField(opts, "argname")
    
    # Positional arguments and options are kept separately, since they are
    # matched and formatted in quite different ways
    argInfo <- data.frame(name=character(0), format=character(0), multiple=logical(0), required=logical(0), stringsAsFactors=FALSE)
    optInfo <- data.frame(name=character(0), label=character(0), format=character(0), required=logical(0), stringsAsFactors=FALSE)
    
    if (length(spec) > 0) {
        argMatches <- ore_search("^(\\w+)(\\.\\.\\.)?(\\?)?$", unlist(spec), simplify=FALSE)
        argInfo <- do.call(rbind, lapply(argMatches, function (m) {
            if (is.null(m))
                stop("Format of positional arguments is invalid")
            else
                data.frame(name=m[,1], format=m[,1], multiple=!is.na(m[,2]), required=is.na(m[,3]), stringsAsFactors=FALSE)
        }))
        
        nargs <- nrow(argInfo)
        if (any(argInfo$multiple[-nargs]))
            stop("Only the last positional argument can take multiple values")
        firstOptional <- match(FALSE, argInfo$required)
        if (!is.na(firstOptional) && any(argInfo$required[-seq_len(firstOptional)]))
            stop("Required positional arguments cannot follow optional ones")
    }
    
    if (!is.null(attr(spec, "options"))) {
        labels <- trimws(unlist(ore_split(",", attr(spec, "options"))))
        labels <- labels[nzchar(labels)]
        
        for (label in labels) {
            longMatch <- ore_search("^([\\w-]+)(!)?$", label)
            index <- if (is.null(longMatch)) NA_integer_ else match(longMatch[,1], optLong)
            if (!is.na(index)) {
                optLabels <- paste0("--", optLong[index])
                format <- paste0(optLabels, ifelse(optArg[index], paste0("=<",optArgname[index],">"), ""))
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
                optLabels <- paste0("-", optShort[index])
                format <- paste0(optLabels, ifelse(optArg[index], paste0(" <",optArgname[index],">"), ""))
                required <- !is.na(shortMatches[,2])
            }
            optInfo <- rbind(optInfo, data.frame(name=optName[index], label=optLabels, format=format, required=required, stringsAsFactors=FALSE))
        }
    }
    
    return (structure(list(args=argInfo, options=optInfo), class="arrgPattern"))
}

matchPattern <- function (pattern, parsed, defaults)
{
    result <- list()
    
    # Every option given must be one that this pattern accepts. The label used
    # is the one the user actually typed, which may be the short or long form
    unexpected <- setdiff(names(parsed$options), pattern$options$name)
    if (length(unexpected) > 0)
        return (mismatch(es("option #{parsed$labels[[unexpected[1]]]} is not valid here")))
    
    args <- pattern$args
    nargs <- nrow(args)
    ngiven <- length(parsed$args)
    nrequired <- sum(args$required)
    
    if (ngiven < nrequired)
        return (mismatch(es("argument <#{args$format[ngiven+1]}> is required")))
    if (ngiven > nargs && !any(args$multiple))
        return (mismatch(es("too many arguments (#{ngiven} given, #{nargs} expected at most)")))
    
    for (i in seq_len(nargs)) {
        if (i > ngiven)
            break   # An optional argument that wasn't given is left unset
        else if (args$multiple[i])
            result[[args$name[i]]] <- parsed$args[i:ngiven]
        else
            result[[args$name[i]]] <- parsed$args[i]
    }
    
    for (i in seq_len(nrow(pattern$options))) {
        name <- pattern$options$name[i]
        if (!is.null(parsed$options[[name]]))
            result[[name]] <- parsed$options[[name]]
        else if (pattern$options$required[i])
            return (mismatch(es("option #{pattern$options$label[i]} is required")))
        else if (!is.null(defaults[[name]]))
            result[[name]] <- defaults[[name]]
    }
    
    return (result)
}

formatPattern <- function (pattern)
{
    elements <- character(0)
    
    opts <- pattern$options
    if (nrow(opts) > 0)
        elements <- c(elements, ifelse(opts$required, opts$format, paste0("[",opts$format,"]")))
    
    args <- pattern$args
    if (nrow(args) > 0)
        elements <- c(elements, paste0(ifelse(args$required,"<","[<"), args$format, ">", ifelse(args$multiple,"...",""), ifelse(args$required,"","]")))
    
    paste(elements, collapse=" ")
}
