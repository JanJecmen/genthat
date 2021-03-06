context("run-package")

test_that("run package works", {
    withr::with_temp_libpaths({
        devtools::install_local("samplepkg", quiet=TRUE, build_vignettes=TRUE, lib=.libPaths()[1])


        ret <- run_package("samplepkg", types="examples", quiet=TRUE)

        expect_length(ret, 1)
        expect_equal(ret$examples, c("My-add.Rd.R"=0, "My-call.Rd.R"=0, "My-public.Rd.R"=0))

        ret <- run_package("samplepkg", types="tests", quiet=TRUE)

        expect_length(ret, 1)
        expect_equal(ret$tests, c("testthat.R"=0))

        ret <- run_package("samplepkg", types="vignettes", quiet=TRUE)

        expect_length(ret, 1)
        expect_equal(ret$vignettes, c("my-ext-vignette-notrace.R"=0, "my-ext-vignette-trace.R"=0, "my-vignette.R"=0))

        ret <- run_package("samplepkg", quiet=TRUE)

        expect_length(ret, 3)
        expect_equal(ret$examples, c("My-add.Rd.R"=0, "My-call.Rd.R"=0, "My-public.Rd.R"=0))
        expect_equal(ret$tests, c("testthat.R"=0))
        expect_equal(ret$vignettes, c("my-ext-vignette-notrace.R"=0, "my-ext-vignette-trace.R"=0, "my-vignette.R"=0))
    })
})
