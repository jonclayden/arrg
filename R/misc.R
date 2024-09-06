"%as%" <- function (X, Y)
{
    storage.mode(X) <- Y
    return (X)
}

wrap <- function (str, width, indent = 0)
{
    if (indent >= width)
        stop(es("Width (#{width}) must be greater than the indent (#{indent})"))
    if (length(str) > 0) {
        str <- ore_subst(es("(\\X{1,#{width-indent}})\\s+"), "\\1\n", str, all=TRUE)
        cat(paste0(rep(" ",indent), str), "\n\n")
    }
}
