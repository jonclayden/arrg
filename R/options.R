# Extract a single field from each of a list of option records, as a vector
optField <- function (opts, field, type = character(1))
{
    vapply(opts, function (o) o[[field]], type, USE.NAMES=FALSE)
}

#' Specify an option in long or short form
#' 
#' This function specifies an option that is accepted by an argument parser.
#' The results of one or more calls to this function are typically passed to
#' [arrg()].
#' 
#' @param label A short-form (single character) and/or long-form label for the
#'   option, specified comma-separated in a single string. At most one of each
#'   form must be given. Long-form labels may be internally hyphenated, as in
#'   "dry-run". Leading hyphens and surrounding whitespace are optional, and
#'   will be stripped.
#' @param description A textual description of the option, for use in the usage
#'   summary.
#' @param arg The name of the option's argument, if it takes one. Otherwise
#'   `FALSE`, indicating no argument. If a `default` is given then the option
#'   takes an argument whatever the value of this parameter, and the argument
#'   will be named after the option unless a name is given here.
#' @param default A default value for the argument, if one is accepted. This
#'   does not have to be a string, and arguments will be coerced to match the
#'   mode of the default when parsed. The default value of `NULL` means that no
#'   default is specified: an option taking an argument will then default to
#'   `NA`, and an option taking no argument to `FALSE`.
#' @return A list of class `"arrgOption"` giving details of the option. This
#'   will not usually be used directly, but passed to [arrg()].
#' @seealso [arrg()]
#' 
#' @examples
#' # A simple flag-style option with no argument
#' opt("h,help", "Display this usage information and exit")
#' 
#' # An option that takes an integer argument called "count"
#' opt("n,times", "Run this many times", arg="count", default=1L)
#' 
#' @author Jon Clayden
#' @export
opt <- function (label, description, arg = FALSE, default = NULL)
{
    if (missing(description))
        stop("A description must be given for each option")
    if (!is.character(description) || length(description) != 1L || is.na(description))
        stop("An option description must be a single string")
    if (!is.character(label) || length(label) == 0L)
        stop("An option label must be a string")
    
    labels <- trimws(unlist(ore_split(ore(",",syntax="fixed"), label)))
    labels <- ore_subst("^-+", "", labels)
    shortForm <- labels %~% "^\\w$"
    if (!all(labels[!shortForm] %~% "^\\w+(-\\w+)*$"))
        stop("Option labels must be alphanumeric, and may be internally hyphenated")
    if (length(labels) == 0L || sum(shortForm) > 1L || sum(!shortForm) > 1L)
        stop("Too few or too many labels for option")
    
    argname <- NA_character_
    if (is.character(arg))
    {
        if (length(arg) != 1L || is.na(arg))
            stop("An option's argument name must be a single string")
        argname <- arg
        arg <- TRUE
    }
    if (!is.logical(arg) || length(arg) != 1L || is.na(arg))
        stop("An option's argument must be named, or FALSE if it takes none")
    
    # The name used to key the option in parsed output: the long label if there
    # is one, otherwise the short label
    name <- labels[which.max(nchar(labels))]
    
    # Specifying a default implies that the option takes an argument. Storing
    # each option separately (rather than in a shared data frame) means that
    # defaults keep their own modes, whatever other options are specified
    if (!is.null(default))
    {
        arg <- TRUE
        mode <- storage.mode(default)
    }
    else if (arg)
    {
        default <- NA_character_
        mode <- "character"
    }
    else
    {
        default <- FALSE
        mode <- "logical"
    }
    
    if (arg && is.na(argname))
        argname <- name
    
    structure(list(short=if (any(shortForm)) labels[shortForm] else NA_character_,
                   long=if (any(!shortForm)) labels[!shortForm] else NA_character_,
                   name=name,
                   description=description,
                   arg=arg,
                   argname=argname,
                   default=default,
                   mode=mode),
              class="arrgOption")
}
