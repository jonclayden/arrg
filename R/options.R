opt <- function (label, description, arg = FALSE, default = NA_character_)
{
    label <- ore_split(ore("|",syntax="fixed"), label)
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
               mode=storage.mode(default))
}
