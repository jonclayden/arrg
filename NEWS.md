This file documents the significant user-visible changes in each release of the `arrg` R package.

# arrg 0.2.0

## New features

- Parsers gain a new `run()` method, which accepts a function or code block that does the core work
  of a script. This acts as a single entry point for scripting and interactive use. When run in a
  script, it parses the arguments, answers `--help` with the usage summary, reports a usage error
  on standard error with a non-zero exit status, and otherwise runs the provided code. When a file
  written that way is `source()`d rather than run from a command line, `run()` runs nothing. It
  returns a function instead, whose arguments correspond to the command's options and positional
  arguments.

- `library(arrg)` is no longer necessary. `opt()` and `pat()` are made available within the call
  to `arrg()` itself, so a script need refer only to `arrg::arrg()`.

- A help option, and a usage pattern for it, are now provided automatically unless the command
  declares one of its own. A string may be passed as the new `help` argument to `arrg()` to
  replace its description, or `FALSE` to suppress it altogether.

- If no usage pattern is given, one is now generated that accepts every option the command
  declares, and any number of positional arguments, which are named `args`. A simple command
  therefore needs nothing beyond its `opt()` calls.

- A pattern may be given `.options=TRUE` to accept every option the command declares, instead of
  naming each of them.

- A single `pat()` may now be passed to `arrg()` in place of a list containing one.

- Positional arguments may now have default values, given as named elements of `pat()`, as in
  `pat(path=".")`. Such an argument is optional, and a value given for it is coerced to the mode of
  the default, as already happened for options.

- Options and positional arguments may now be interleaved, so `cmd file -v` is accepted as well as
  `cmd -v file`.

- Long-form option labels may now contain hyphens, as in `--dry-run`.

- A lone `-` is now treated as a positional argument, following the usual convention by which it
  refers to standard input.

- An option's argument may now be given as empty, as in `--output=`.

- Default values for option arguments are now shown in the usage summary, appended to each option's
  description as `[default <value>]`, with strings quoted.

- The parser returned by `arrg()` now records the command's name, as its `name` element.

- Parsers now print their usage information when their name is typed at the console, rather than
  the contents of the object itself.

## Breaking changes

- The `options` argument to `pat()` is now called `.options`, which distinguishes it from the
  positional arguments passed through `...`. Use of the old name is reported as an error for the
  time being.

- The `args` argument to the parser's `parse()` method is now called `argv`.

- An optional positional argument that was not given is now absent from the parsed result, rather
  than present as `NA`. Test for it with `is.null()`, or give it a default value.

- Because options and positional arguments may now be interleaved, an argument that looks like an
  option is parsed as one even where it follows a positional argument. Precede it with `--` to pass
  it through untouched.

- Since a help option is now provided automatically, a command that did not declare one gains `-h`
  and `--help`, and a further line in its usage summary. Pass `help=FALSE` to `arrg()` to prevent
  this.

- Option labels must be alphanumeric, hyphenated only in long form. Leading hyphens and surrounding
  whitespace are stripped, as was already documented but not previously done.

## Bug fixes

- The `--` terminator now works fully. Previously the arguments after it were still taken apart as
  though they might be option clusters, so `cmd -- -tn3` would split `-tn3` rather than passing it
  through as a positional argument.

- Default values no longer interfere with one another. Previously a command that combined a flag
  with both an integer-valued and a string-valued option would report the flag's default as `NA`
  instead of `FALSE`.

- The `show()` method no longer fails with "argument 1 (type 'list') cannot be handled by 'cat'"
  when the usage patterns wrap to differing numbers of lines.

- A parser with no options at all is now usable. Previously both `show()` and `parse()` failed with
  "argument is of length zero".

- An unrecognised short option is now reported as it was given. Previously, for any command with a
  long-only option, `-NAv` would be reported as an unexpected `-N`.

- An unrecognised letter within a cluster is now an error. Previously the rest of the cluster was
  quietly taken as a positional argument, so `cmd -tx` behaved as `cmd -t x`.

- Duplicate option labels are now reported when the parser is created, rather than appearing later
  as an unexpected option.

- An option that takes an argument by virtue of having been given a default now has an argument
  name, rather than appearing as `<NA>` in the usage summary.

- Positional arguments beyond those a pattern allows are now reported. Previously a pattern that
  declared none would silently discard them, so `cmd -h extra` ignored `extra`.

- An invalid value for an option is now reported against that option, naming the type expected,
  rather than producing R's "NAs introduced by coercion" warning and an `NA` value.

- A pattern in which a required positional argument follows an optional one is now rejected when
  the parser is created. Previously the required argument was quietly left as `NA`.

- Usage summaries no longer degenerate into one word per line in narrow terminals, or where the
  option labels are long. A label taking up more than 60% of the available width is split across
  two lines, and descriptions are placed beneath their labels when there is no room for a column
  alongside.

- A default value of length other than one no longer causes an error in `opt()`.

- An empty `.options` string in a pattern no longer causes an error.

- Failure to match any usage pattern now reports each pattern along with the reason it was
  rejected, rather than only that nothing matched.

- Omitting the command's name, so that the first argument to `arrg()` is an option or a pattern, is
  now reported. Previously the parser was built regardless, with the fields of the misplaced object
  standing in for the command name, producing a nonsensical usage summary and quietly discarding
  the option.

# arrg 0.1.0

- First public release.
