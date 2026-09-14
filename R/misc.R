"%as%" <- function (X, Y)
{
    storage.mode(X) <- Y
    return (X)
}

# Coerce a value given for an option to the mode of the option's default,
# reporting any failure in terms of the option rather than leaving R to emit a
# bare coercion warning. Coercion to logical mode fails silently, so the result
# is tested rather than a warning being caught
coerceValue <- function (value, mode, label)
{
    result <- withCallingHandlers(value %as% mode,
                                  warning=function (cond) invokeRestart("muffleWarning"))
    if (is.na(result) && !is.na(value))
        stop(es("Value \"#{value}\" is not valid for option #{label}, which takes an argument of type #{mode}"))
    return (result)
}
