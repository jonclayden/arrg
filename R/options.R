#' Specify an option in long or short form
#' 
#' This function specifies an option that is accepted by an argument parser.
#' The results of one or more calls to this function are typically passed to
#' [arrg()].
#' 
#' @param label A short-form (single character) and/or long-form label for the
#'   option, specified comma-separated in a single string. At most one of each
#'   form must be given. Leading hyphens are optional.
#' @param description A textual description of the option, for use in the usage
#'   summary.
#' @param arg The name of the option's argument, if it takes one. Otherwise
#'   `FALSE`, indicating no argument.
#' @param default A default value for the argument, if one is accepted. This
#'   does not have to be a string, and arguments will be coerced to match the
#'   mode of the default when parsed. If the option takes no argument the
#'   default value will be `FALSE`.
#' @return A data frame giving details of the option. This will not usually be
#'   used directly, but passed to [arrg()].
#' @seealso [arrg()]
#' 
#' @examples
#'   # A simple flag-style option with no argument
#'   opt("h,help", "Display this usage information and exit")
#'   
#'   # An option that takes an integer argument called "count"
#'   opt("n,times", "Run this many times", arg="count", default=1L)
#' 
#' @author Jon Clayden
#' @export
opt <- function (label, description, arg = FALSE, default = NA_character_)
{
    label <- ore_split(ore(",",syntax="fixed"), label)
    shortForm <- label %~% "^-?\\w$"
    if (length(label) == 0L || sum(shortForm) > 1L || sum(!shortForm) > 1L)
        stop("Too few or too many labels for option")
    
    argname <- NA_character_
    if (is.character(arg)) {
        argname <- arg
        arg <- TRUE
    }
    arg <- arg || !is.na(default)
    if (!arg) default <- FALSE
    
    data.frame(short=ifelse(any(shortForm),label[shortForm],NA),
               long=ifelse(any(!shortForm),label[!shortForm],NA),
               name=label[which.max(nchar(label))],
               description=description,
               arg=arg,
               argname=argname,
               default=default,
               mode=storage.mode(default),
               stringsAsFactors=FALSE)
}
