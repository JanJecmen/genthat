#' @export
get_latest_package_version <- function(package, repos=getOption("repos"), type="source") {
    contrib_url <- contrib.url(repos, type)
    available_pkgs <- available.packages(contrib_url)

    if (pkg %in% row.names(available_pkgs)) {
        available_pkgs[pkg, "Version"]
    } else {
        NULL
    }
}

#' @export
download_package <- function(package, destdir, version=NULL, repos=getOption("repos"), extract=TRUE, overwrite=FALSE, verbose=TRUE, ...) {
    contrib_url <- contrib.url(repos, "source")

    if (is.null(version)) {
        version <- get_latest_package_version(package, repos, "source")
        if (is.null(version)) {
            stop("Package ", package, " is not in the CRAN available packages (", repos, ") and no version was specified")
        }
    } else {
        # only the latest versions are kept in the CRAN top level
        # the rest is in the Archive
        contrib_url <- sprintf("%s/Archive/%s", contrib_url, package)
    }

    archive <- sprintf("%s_%s.tar.gz", package, version)
    url <- sprintf("%s/%s", contrib_url, archive)

    if (!dir.exists(destdir)) {
        stop("Destination directory ", destdir," does not exists")
    }

    destfile <- file.path(destdir, archive)
    if (file.exists(destfile)) {
        if (overwrite) {
            if (verbose) {
                cat("Removing package archive", destfile)
            }
            file.remove(destfile)
        } else {
            stop("File already exists ", destfile)
        }
    }

    if (verbose) {
        cat("Downloading package", package, "version", version ," from ", url, "\n")
    }

    if (download.file(url, destfile=destfile, quiet=!verbose, ...)) {
        stop("Download from ", url, " failed")
    }

    if (extract) {
        pkgdir <- file.path(destdir, package)

        if (dir.exists(pkgdir)) {
            if (overwrite) {
                if (verbose) {
                    cat("Removing directory ", pkgdir)
                }
                unlink(pkgdir, recursive=TRUE)
            } else {
                stop("Destination directory for extracting exists ", pkgdir)
            }
        }

        if (verbose) {
            cat("Extracing", archive ," to ", pkgdir, "\n")
        }

        untar(destfile, exdir=destdir, verbose=verbose)
    }

    return(devtools::as.package(pkgdir))
}

#' @export
run_genthat_tests <- function(pkgdir, genthatdir) {
    stopifnot(dir.exists(pkgdir))
    stopifnot(dir.exists(genthatdir))

    withr::with_temp_libpaths({
        devtools::install_dev_deps(pkgdir)
        install.packages(pkgdir, repos=NULL)

        files <- list.files(pattern='\\.R$', path=genthatdir, recursive=TRUE, full.names=TRUE)
        if (!length(files)) {
            warning("No tests found in ", genthatdir)

            return(empty_test_results())
        }

        reporter <- CapturingListReporter$new()
        testthat::set_reporter(reporter)

        lapply(files, function(x) {
            # this is why we do test_file
            # it is stupid, but the they only call the start_file
            # on the reported created internally that is used to return the result
            reporter$start_file(x)
            testthat::test_file(x, reporter=reporter)
        })

        reporter$get_results()
    })
}
