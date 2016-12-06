CapturingListReporter <- R6::R6Class("CapturingListReporter", inherit = testthat::ListReporter,
  public = list(
    output = NULL,
    current_con = NULL,

    initialize = function() {
        super$initialize()

        self$output <- tempfile()
    },

    start_test = function(context, test) {
        super$start_test(context, test)

        self$current_con <- file(self$output, "w+")
        stopifnot(isOpen(self$current_con))

        sink(self$current_con, type="output", append=TRUE)
        sink(self$current_con, type="message", append=TRUE)
    },

    end_test = function(context, test) {
        sink(NULL, type="output")
        sink(NULL, type="message")

        close(self$current_con)

        elapsed <- as.double(proc.time() - self$current_start_time)

        out <- paste(readLines(self$output, warn=FALSE), collapse="\n")
        file.remove(self$output)

        self$results$push(list(
            file =    if (!is.null(self$current_file)) self$current_file else NA_character_,
            context = context,
            test =    test,
            user =    elapsed[1],
            system =  elapsed[2],
            real =    elapsed[3],
            results = self$current_expectations$as_list(),
            output =  out
        ))
    },

    get_results = function() {
        results <- super$get_results()
        class(results) <- c("capturing_testthat_results", class(results))
        results
    }
  )
)

#' @export
as.data.frame.capturing_testthat_results <- function(r) {
    if (length(r) == 0) {
        return(data.frame())
    }

    outputs <- sapply(r, USE.NAMES=FALSE, function(x) x$output)
    messages <- sapply(r, USE.NAMES=FALSE, function(x) {
        # the summary is per test
        # a test can have multiple expectations and thus more messages
        # we need to extract the ones that are associated with failures
        # and concatenate together
        ms <- lapply(x[["results"]], function(y) if ("expectation_success" %in% class(y)) c("") else y[["message"]])
        paste(ms, collapse="\n")
    })

    df <- testthat:::as.data.frame.testthat_results(r)
    cbind(df, output=outputs, messages=messages, stringsAsFactors=FALSE)
    # class(df) <- c("data.frame")
    # df
}

test_package_examples <- function(pkgdir) {
    stopifnot(dir.exists(pkgdir))

    files <- list.files(pattern='\\.Rd$', path=file.path(pkgdir, "man"), recursive=TRUE, full.names=TRUE)
    tmpdir <- tempdir()

    # extract R code from the examples into temporary files
    tmps <- lapply(files, function(x) {
        tmp <- file.path(tmpdir, paste0(tools::file_path_sans_ext(basename(x)), ".R"))
        tools::Rd2ex(x, out=tmp, commentDontrun = TRUE, commentDonttest = FALSE)
    })

    tmps <- as.character(tmps)
    tmps <- tmps[file.exists(tmps)]

    with_capturing_reporter(test_R_files(tmps))
}

test_package_tests <- function(pkgdir) {
    stopifnot(dir.exists(pkgdir))

    if (!devtools::uses_testthat(pkgdir)) {
        testdirs <- c(file.path(pkgdir, "tests"), file.path(pkgdir, "inst"))
        files <- list.files(pattern='\\.R$', path=testdirs, recursive=TRUE, full.names=TRUE)

        with_capturing_reporter(test_R_files(files))
    } else {
        testpaths <- c(file.path(pkgdir, "tests", "testthat"), file.path(pkgdir, "inst", "tests"))
        testdirs <- testpaths[file.exists(testpaths)]
        if (!length(testdirs)) {
            warning("No tests found")
        }

        reporter <- CapturingListReporter$new()
        testthat::test_dir(testdirs[1], reporter=reporter)
        reporter$get_results()
    }
}

test_package_vignettes <- function(pkgdir) {
    stopifnot(dir.exists(pkgdir))

    files <- list.files(pattern='\\.R$', path=file.path(pkgdir, "inst", "doc"), recursive=TRUE, full.names=TRUE)

    with_capturing_reporter(test_R_files(files))
}

#' @export
test_package <- function(pkgdir, types=c("examples", "tests", "vignettes")) {
    stopifnot(dir.exists(pkgdir))

    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok = TRUE)

    result <- list()

    withr::with_temp_libpaths({
        devtools::install_dev_deps(pkgdir)
        install.packages(pkgdir, repos=NULL)

        if(!require(as.package(pkgdir)$package, character.only=TRUE)) {
            stop("Unable to load package", as.package(pkgdir)$package)
        }

        if ("examples" %in% types) {
            result$examples <- test_package_examples(pkgdir)
        }

        if ("tests" %in% types) {
            result$tests <- test_package_tests(pkgdir)
        }

        if ("vignettes" %in% types) {
            result$vignettes <- test_package_vignettes(pkgdir)
        }
    })

    result <- lapply(names(result), function(x) {
        if (length(x) == 0) {
            x <- empty_test_results()
        }

        result[[x]]["type"] <- x;
        result[[x]]
    })

    do.call(rbind, result)
}

with_capturing_reporter <- function(block) {
    reporter <- CapturingListReporter$new()
    testthat::set_reporter(reporter)

    block

    reporter$get_results()
}

test_R_files <- function(files, ...) {
    lapply(files, test_R_file, ...)
}

test_R_file <- function(file, verbose=FALSE) {
    if (!file.exists(file)) {
        stop("File ", file, " does not exist")
    }

    old_gd <- options(device=pdf)
    on.exit({
        options(device=old_gd)
    })

    name <- basename(file)

    if (verbose) {
        cat("Running ", file, "\n")
    }

    # TODO: check if it is the right reporter
    testthat::get_reporter()$start_file(name)
    # wrap it as a single test
    result <- testthat::test_that(name, {
        tryCatch({
            source(file, echo=TRUE, print.eval=TRUE, chdir=TRUE, max.deparse.length=300)

            cat("\n\nExecution of:", file, "succeeded\n")

            testthat::succeed()
        }, error=function(e) {
            cat("\n\nError:\n")
            print(e)
            cat("\nExecution of:", file, "failed\n")

            testthat::fail()
        }, warning=function(e) {
            cat("\n\nWarning:")
            print(e)
            cat("\nExecution of:", file, "succeeded\n")

            testthat::succeed()
        })
    })

    if (verbose) {
        if (result) {
            cat("Running of ", file, " was successfull\n")
        } else {
            cat("Running of ", file, " failed\n")
        }
    }

    invisible()
}

empty_test_results <- function() {
    data.frame(
        file=NA,
        context=NA,
        test=NA,
        nb=0,
        failed=0,
        skipped=FALSE,
        error=FALSE,
        warning=0,
        user=0,
        system=0,
        real=0,
        output=NA,
        messages=NA)
}
