test_package_tests <- function(pkgdir) {
    stopifnot(file.exists(pkgdir))

    devtools::load_all(pkgdir)

    if (!devtools::uses_testthat(pkgdir)) {
        cat("tracing non-testthat tests\n")

        testdirs <- c(file.path(pkgdir, "tests"), file.path(pkgdir, "inst"))
        files <- list.files(pattern='\\.R$', path=testdirs, recursive=TRUE, full.names=TRUE)

        with_list_reporter(test_R_files(files))
    } else {
        testpaths <- c(file.path(pkgdir, "tests", "testthat"), file.path(pkgdir, "inst", "tests"))
        testdirs <- testpaths[file.exists(testpaths)]
        if (!length(testdirs)) {
            cat("No tests found\n")
        } else {
            cat("tracing testthat tests\n")
        }

        reporter <- testthat::ListReporter$new()
        testthat::test_dir(testdirs[1], reporter=reporter)
        reporter$get_results()
    }
}

test_package_vignettes <- function(pkgdir) {
    cat("tracing vignettes\n")

    files <- list.files(pattern='\\.R$', path=file.path(pkgdir, "inst", "doc"), recursive=TRUE, full.names=TRUE)

    devtools::load_all(pkgdir)
    with_list_reporter(test_R_files(files))
}

test_package_examples <- function(pkgdir) {
    cat("tracing examples\n")

    files <- list.files(pattern='\\.Rd$', path=file.path(pkgdir, "man"), recursive=TRUE, full.names=TRUE)
    tmpdir <- tempdir()

    # extract R code to a temporary files
    tmps <- lapply(files, function(x) {
        tmp <- file.path(tmpdir, paste0(tools::file_path_sans_ext(basename(x)), ".R"))
        tools::Rd2ex(x, out=tmp, commentDontrun = TRUE, commentDonttest = FALSE)
    })
    tmps <- as.character(tmps)
    tmps <- tmps[file.exists(tmps)]

    devtools::load_all(pkgdir)
    with_list_reporter(test_R_files(tmps))
}

with_list_reporter <- function(block) {
    reporter <- testthat::ListReporter$new()
    testthat::set_reporter(reporter)

    block

    reporter$get_results()
}


test_R_files <- function(files) {
    lapply(files, test_R_file)
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

    tryCatch({
        source(file, echo=TRUE, print.eval=verbose, chdir=TRUE)

        cat("Execution of", file, "succeeded\n")
        testthat::test_that(name, testthat::succeed("OK"))

        return(TRUE)
    }, error=function(e) {
        cat("Execution of", file, "failed:", e$message, "\n")
        print(e)

        testthat::test_that(name, testthat::fail("Failed"))

        return(FALSE)
    })
}
