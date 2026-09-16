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
#'   question mark if the argument is also optional. An argument may instead be
#'   given as a named element, as in `pat(path=".")`, in which case the name is
#'   the specification and the value is a default. Such an argument is
#'   optional, and a value given for it will be coerced to the mode of the
#'   default, as for [opt()].
#' @param .options A string naming the long or short labels of options that can
#'   be specified with this pattern, comma-separated. Short form options may be
#'   given in one letter cluster for convenience. Options are only required if
#'   followed by an exclamation mark. Alternatively `TRUE`, meaning every
#'   option that the command declares, all of them optional, except any that
#'   [arrg()] generated itself, such as an automatic help option. The leading
#'   period distinguishes this parameter from the positional arguments passed
#'   in `...`, whose names can never contain one.
#' @return A list capturing the positional arguments, with options in an
#'   attribute. This will not usually be used directly, but passed to [arrg()].
#' @seealso [arrg()]
#' 
#' @examples
#'   # A pattern with no positional arguments, but requiring the -h flag
#'   pat(.options="h!")
#'   
#'   # A pattern that takes a command and variable number of arguments, and
#'   # accepts the -n and -t options (note the latter are specified in cluster
#'   # form, but "n,t" is also valid) 
#'   pat("command", "arg...?", .options="nt")
#'   
#'   # A pattern with one optional argument, which defaults to "." if it is
#'   # not given, and which accepts every option the command declares
#'   pat(path=".", .options=TRUE)
#' 
#' @author Jon Clayden
#' @export
pat <- function (..., .options = NULL)
{
    args <- list(...)
    # Transitional: ".options" was originally called "options", which would
    # otherwise now be quietly taken as a positional argument with a default
    if ("options" %in% names(args))
        stop("The \"options\" argument to pat() is now called \".options\"")
    return (structure(args, options=.options, class="arrgPatternSpec"))
}

# An indication that a pattern did not match, with the reason why, as distinct
# from a list of matched arguments
mismatch <- function (reason)
{
    return (structure(list(reason=reason), class="arrgMismatch"))
}

resolvePattern <- function (spec, opts, generated = logical(length(opts)))
{
    optShort <- optField(opts, "short")
    optLong <- optField(opts, "long")
    optName <- optField(opts, "name")
    optArg <- optField(opts, "arg", logical(1))
    optArgname <- optField(opts, "argname")
    
    # Positional arguments and options are kept separately, since they are
    # matched and formatted in quite different ways
    argInfo <- data.frame(name=character(0), format=character(0), multiple=logical(0), required=logical(0), mode=character(0), stringsAsFactors=FALSE)
    optInfo <- data.frame(name=character(0), label=character(0), format=character(0), required=logical(0), stringsAsFactors=FALSE)
    
    # Defaults are held in a list rather than alongside the rest of the
    # argument information, so that each keeps its own mode
    argDefaults <- list()
    
    if (length(spec) > 0) {
        # An argument given as a named element takes its format from the name
        # and its default value from the element itself; one given unnamed is
        # just the format, and has no default
        defaulted <- if (is.null(names(spec))) logical(length(spec)) else nzchar(names(spec))
        formats <- character(length(spec))
        formats[defaulted] <- names(spec)[defaulted]
        if (any(!defaulted)) {
            unnamed <- spec[!defaulted]
            if (!all(vapply(unnamed, function (x) is.character(x) && length(x) == 1L, logical(1))))
                stop("Format of positional arguments is invalid")
            formats[!defaulted] <- unlist(unnamed)
        }
        
        argMatches <- ore_search("^(\\w+)(\\.\\.\\.)?(\\?)?$", formats, simplify=FALSE)
        argInfo <- do.call(rbind, mapply(function (m, default, hasDefault) {
            if (is.null(m))
                stop("Format of positional arguments is invalid")
            else
                data.frame(name=m[,1], format=m[,1], multiple=!is.na(m[,2]),
                           required=is.na(m[,3]) && !hasDefault,
                           mode=if (hasDefault) storage.mode(default) else "character",
                           stringsAsFactors=FALSE)
        }, argMatches, spec, defaulted, SIMPLIFY=FALSE))
        
        argDefaults <- spec[defaulted]
        names(argDefaults) <- argInfo$name[defaulted]
        
        npositional <- nrow(argInfo)
        if (any(argInfo$multiple[-npositional]))
            stop("Only the last positional argument can take multiple values")
        firstOptional <- match(FALSE, argInfo$required)
        if (!is.na(firstOptional) && any(argInfo$required[-seq_len(firstOptional)]))
            stop("Required positional arguments cannot follow optional ones")
    }
    
    # A row of option information, formatted in whichever style was asked for
    optRow <- function (index, useShort, required) {
        # Note that useShort must match the length of index, since ifelse()
        # returns a value shaped like its test rather than its branches
        useShort <- rep_len(useShort, length(index))
        labels <- ifelse(useShort, paste0("-",optShort[index]), paste0("--",optLong[index]))
        formats <- paste0(labels, ifelse(optArg[index],
                                         ifelse(useShort, paste0(" <",optArgname[index],">"),
                                                          paste0("=<",optArgname[index],">")),
                                         ""))
        data.frame(name=optName[index], label=labels, format=formats, required=required, stringsAsFactors=FALSE)
    }
    
    if (isTRUE(attr(spec, "options"))) {
        # Every option the command declares, other than any generated for it,
        # preferring the short form of each where there is one
        index <- which(!generated)
        if (length(index) > 0)
            optInfo <- rbind(optInfo, optRow(index, !is.na(optShort[index]), FALSE))
    } else if (!is.null(attr(spec, "options"))) {
        labels <- trimws(unlist(ore_split(",", attr(spec, "options"))))
        labels <- labels[nzchar(labels)]
        
        for (label in labels) {
            longMatch <- ore_search("^([\\w-]+)(!)?$", label)
            index <- if (is.null(longMatch)) NA_integer_ else match(longMatch[,1], optLong)
            if (!is.na(index)) {
                optInfo <- rbind(optInfo, optRow(index, FALSE, !is.na(longMatch[,2])))
            } else {
                # Not a known long-form label, so treat it as a cluster of
                # short-form ones, each optionally followed by an exclamation
                if (!(label %~% "^(\\w!?)+$"))
                    stop("Invalid option specification in pattern: ", label)
                shortMatches <- ore_search("(\\w)(!)?", label, all=TRUE)
                if (!all(shortMatches[,1] %in% optShort))
                    stop("Pattern uses options not included in the main specification")
                index <- match(shortMatches[,1], optShort)
                optInfo <- rbind(optInfo, optRow(index, TRUE, !is.na(shortMatches[,2])))
            }
        }
    }
    
    return (structure(list(args=argInfo, options=optInfo, defaults=argDefaults), class="arrgPattern"))
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
    nexpected <- nrow(args)
    ngiven <- length(parsed$args)
    nrequired <- sum(args$required)
    
    if (ngiven < nrequired)
        return (mismatch(es("argument <#{args$format[ngiven+1]}> is required")))
    if (ngiven > nexpected && !any(args$multiple))
        return (mismatch(es("too many arguments (#{ngiven} given, #{nexpected} expected at most)")))
    
    for (i in seq_len(nexpected)) {
        name <- args$name[i]
        if (i <= ngiven) {
            value <- if (args$multiple[i]) parsed$args[i:ngiven] else parsed$args[i]
            # Unlike an option, whose mode is the same in every pattern, a
            # positional argument of the wrong mode only rules out this one
            coerced <- tryCatch(coerceValue(value, args$mode[i], es("argument <#{args$format[i]}>")),
                                error=function (cond) mismatch(conditionMessage(cond)))
            if (inherits(coerced, "arrgMismatch"))
                return (coerced)
            result[[name]] <- coerced
        } else if (!is.null(pattern$defaults[[name]]))
            result[[name]] <- pattern$defaults[[name]]
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
