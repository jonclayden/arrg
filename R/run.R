# A script run with Rscript or littler is evaluated at the top level, whereas
# one that is source()d appears below a call to source() on the stack. Other
# contexts that evaluate expressions directly, such as knitr, therefore count
# as scripts, which is generally what is wanted
beingSourced <- function ()
{
    isSource <- function (call) {
        fun <- call[[1]]
        if (is.name(fun))
            as.character(fun) %in% c("source", "sys.source")
        else if (is.call(fun) && identical(fun[[1]], quote(`::`)))
            as.character(fun[[3]]) %in% c("source", "sys.source")
        else
            FALSE
    }
    return (any(vapply(sys.calls(), isSource, logical(1))))
}

# Rscript passes a script's arguments through commandArgs(), but littler puts
# them into a top-level variable called "argv"
scriptArgs <- function ()
{
    if (exists("argv", envir=globalenv(), inherits=FALSE)) {
        argv <- get("argv", envir=globalenv())
        if (is.character(argv))
            return (argv)
    }
    return (commandArgs(trailingOnly=TRUE))
}

# Whether the arguments include a request for help. Anything after a "--"
# terminator is a positional argument, and so is not considered
helpRequested <- function (argv, o)
{
    terminator <- match("--", argv, nomatch=length(argv)+1L)
    labels <- c(if (!is.na(o$short)) paste0("-",o$short), if (!is.na(o$long)) paste0("--",o$long))
    return (any(argv[seq_len(terminator-1L)] %in% labels))
}

# Call the body of a script, passing it the parsed arguments as a list, or if
# it takes no arguments, binding those arguments in the environment it runs in
invokeBody <- function (body, parsed, allNames)
{
    if (length(formals(body)) > 0L)
        return (body(parsed))
    
    # The body's own environment is the parent, so that anything else it refers
    # to, such as a function defined alongside it, resolves as usual
    env <- new.env(parent=environment(body))
    for (n in names(parsed))
        assign(n, parsed[[n]], envir=env)
    # Names that the parser could have produced, but didn't, are bound to NULL
    # so that they can be tested with is.null() rather than being unbound
    for (n in setdiff(allNames, names(parsed)))
        assign(n, NULL, envir=env)
    
    environment(body) <- env
    return (body())
}

# The body of a script may be given as a block of code, or as a function of
# one argument or none. A block is turned into a function of no arguments, so
# that it runs in a frame of its own and thereby supports return(), on.exit()
# and anything else that needs one. Note that the expression is not evaluated
# unless it has to be, so that a block is never run just to identify it
resolveBody <- function (expr, envir)
{
    if (is.call(expr) && identical(expr[[1]], quote(`{`))) {
        fun <- function () NULL
        body(fun) <- expr
        environment(fun) <- envir
        return (fun)
    }
    
    fun <- eval(expr, envir)
    if (!is.function(fun))
        stop("The body of a script must be a function, or a block of code in braces")
    return (fun)
}
