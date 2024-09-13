args <- arrg("test",
             opt("h|help", "Display this usage information and exit"),
             opt("n|times", "Run test the specifed number of times", arg="count", default=1L),
             opt("t|time", "Print the overall run-time once the test is completed"),
             opt("install", "Install the code before testing it"),
             patterns=list(pat(options="h!"),
                           pat("command", "arg...?", options="nt"),
                           pat("path?", options="n,t,install")),
             header="Test your code",
             footer="Run the test on the code at the specified path (default \".\"), or run a specific command.")

expect_stdout(args$show(), "usage")

p1 <- args$parse("-h")
p2 <- args$parse(c("-tn3", "--install", "."))       # NB: --install flag is the only thing that marks this as the third pattern not the second
p3 <- args$parse(c("-t", "mycommand", "one", "two"))

expect_true(p1$help)
expect_null(p2$help)
expect_true(p2$time)
expect_equal(p2$times, 3L)
expect_true(p2$install)
expect_equal(p2$path, ".")
expect_true(p3$time)
expect_equal(p3$command, "mycommand")
expect_equal(p3$arg, c("one","two"))
