format_calling_args <- function(args, include_names=TRUE) {
    args <- lapply(args, serialize_value)

    if (include_names && !is.null(names(args))) {
        pairs <- zip(name=names(args), value=args)

        lapply(pairs, function(x) {
            if(!is_empty_str(x$name)) {
                paste0(escape_name(x$name), "=", x$value)
            } else {
                x$value
            }
        })
    } else {
        args
    }
}

dump_raw_trace <- function(trace) {
    s <- utils::capture.output(utils::str(trace))
    s <- paste("# ", s, collapse="\n")
    s <- paste0("# TRACE:\n", s, "\n\n")
    s
}

generate_call <- function(trace) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))

    fun <- trace$fun

    if (is_infix_fun(fun) && trace$pkg == "base") {
        args <- format_calling_args(trace$args, include_names=FALSE)

        if (length(args) != 2) {
            stop("Call to infix function: `", fun,
                 "` does not have exactly two arguments, instead it has: ",
                 paste(args, collapse=", "))
        }

        sep <- if (is_infix_fun_no_space(fun)) "" else " "

        paste(args[[1]], fun, args[[2]], collapse=sep)
    } else {
        args <- format_calling_args(trace$args, include_names=TRUE)
        args_str <- paste(args, collapse=", ")
        fun <- escape_name(fun)
        pkg <- trace$pkg
        pkg <- if (is.null(trace$pkg)) "" else paste0(pkg, ":::")

        paste0(pkg, fun, '(', args_str, ')')
    }
}

#' @title Generate test case code from a trace
#' @description Given a genthat trace it generates a corresponding test case
#'
#' @param trace trace value
#' @param include_trace_dump whether to include raw trace at the beginning of the test
#' the trace is formatted using `str` function.
#'
#' @importFrom utils str
#' @export
generate_test_code <- function(trace, include_trace_dump=FALSE) {
    UseMethod("generate_test_code")
}

#' @export
generate_test_code.genthat_trace <- function(trace, include_trace_dump=FALSE) {
    call <- generate_call(trace)
    globals <- paste(names(trace$globals), lapply(trace$globals, serialize_value), sep=" <- ", collapse="\n")
    retv <- serialize_value(trace$retv)

    header <- "library(testthat)\n\n"
    if (include_trace_dump) {
        header <- paste(header, dump_raw_trace(trace), sep="\n")
    }

    paste0(
        header,
        'test_that("', trace$fun, '", {\n',
        globals,
        '\nexpect_equal(', call, ', ', retv, ')\n})'
    )
}

#' @export
generate_test_code.default <- function(trace, include_trace_dump) {
    NULL
}


#' @title generate test from a trace
#' @description given a trace, it generates a test
#' @return a data frame or a tibble with the following
#' trace      : chr
#' fun        : chr
#' code       : chr (can be NA)
#' error      : chr (can be NA)
#' elapsed    : numeric
#'
#' @export
#'
generate_test <- function(trace, ...) {
    UseMethod("generate_test")
}

generate_test.genthat_trace_entry <- function(trace, ...) {
    list(
        trace=format(trace),
        fun=trace$fun,
        code=NA,
        error="No return value recorded",
        elapsed=NA
    )
}

#' @importFrom utils capture.output
#' @importFrom utils getTxtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils str
generate_test.genthat_trace <- function(trace, ...) {
    result <- list(
        trace=format(trace),
        fun=trace$fun,
        code=NA,
        error=NA,
        elapsed=NA
    )

    # this style is needed so we can set the error field from error handler
    # which would otherwise create a new scope copying the result list
    # on modification
    result <- tryCatch({
        time <- system.time(code <- generate_test_code(trace, ...))
        result$elapsed <- time["elapsed"]

        if (!is.null(code)) {
            result$code <- code
        }

        result
    }, error=function(e) {
        result$error <- e$message
        result
    })

    result
}

#' @title Generates test cases from traces
#'
#' @param traces from which to generate test cases
#' @param show_progress (log) show progress during test generation
#' @param ... additional arguments supplied to `generate_test_code` function.
#'
#' @return a data frame or a tibble with the following
#' fun        : chr
#' trace      : chr
#' trace_type : chr
#' code       : chr (can be NA)
#' error      : chr (can be NA)
#'
#' @description Generates tests cases from the captured traces.
#' @export
#'
generate_tests <- function(traces, quiet=TRUE, ...) {
    stopifnot(is.list(traces) || is.environment(traces))

    if (length(traces) == 0) {
        return(data.frame())
    }

    tests <- lapply(traces, function(x) {
        message(appendLF=FALSE)
        generate_test(x, ...)
    })

    if (requireNamespace("dplyr", quietly=TRUE)) {
        df <- dplyr::bind_rows(tests)
        as_data_frame(df)
    } else {
        message("dplyr is not available, which is a pity since `do.call(rbind, ...)` is super slow")
        do.call(rbind, tests)
    }
}

save_tests <- function(output_dir, tests) {
    stopifnot(is.character(output_dir) && length(output_dir) == 1)
    stopifnot(is.data.frame(tests))

    if (!dir.exists(output_dir)) {
        if (!dir.create(output_dir)) {
            stop("Couldn't create ", output_dir)
        }
    }

    lapply(tests, function(x) {
        fname <- file.path(output_dir, paste0("test-", x$id, ".R"))
        write(x$test, file=fname)
    })
}
