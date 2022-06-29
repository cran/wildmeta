## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(wildmeta)
library(clubSandwich)
library(robumeta)

robu_model <- robu(d ~ 0 + study_type + hrs + test,
                   studynum = study,
                   var.eff.size = V,
                   small = FALSE,
                   data = SATcoaching)

Wald_test_cwb(full_model = robu_model,
              constraints = constrain_equal(1:3),
              R = 99,
              seed = 20201228)

## -----------------------------------------------------------------------------
Wald_test_cwb(full_model = robu_model,
              constraints = constrain_equal(1:3),
              R = 99,
              adjust = "CR2",
              seed = 20201229)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(metafor)

rma_model <- rma.mv(yi = d ~ 0 + study_type + hrs + test,
                    V = V,
                    random = ~ study_type | study,
                    data = SATcoaching,
                    subset = !is.na(hrs) & !is.na(test))

Wald_test_cwb(full_model = rma_model,
              constraints = constrain_equal(1:3),
              R = 99,
              seed = 20210314)

## -----------------------------------------------------------------------------
system.time(
  res <- Wald_test_cwb(full_model = robu_model,
                       constraints = constrain_equal(1:3),
                       R = 1999, 
                       seed = 20201229)
)

## ---- eval = requireNamespace("future", quietly = TRUE) & requireNamespace("parallelly", quietly = TRUE) & requireNamespace("future.apply", quietly = TRUE)----
library(future)

if (parallelly::supportsMulticore()) {
  plan(multicore) 
} else {
  plan(multisession)
}
parallelly::availableWorkers() |> length()

system.time(
  res <- Wald_test_cwb(full_model = robu_model,
                       constraints = constrain_equal(1:3),
                       R = 1999, 
                       seed = 20201229)
)

plan(sequential)

## ---- warning = FALSE---------------------------------------------------------
system.time(
  Wald_test_cwb(full_model = rma_model,
                constraints = constrain_equal(1:3),
                R = 99,
                seed = 20210314)
)


## ---- eval = requireNamespace("future", quietly = TRUE) & requireNamespace("parallelly", quietly = TRUE) & requireNamespace("future.apply", quietly = TRUE)----
library(future)

if (parallelly::supportsMulticore()) {
  plan(multicore) 
} else {
  plan(multisession)
}
parallelly::availableWorkers() |> length()

system.time(
  Wald_test_cwb(full_model = rma_model,
                constraints = constrain_equal(1:3),
                R = 99,
                seed = 20210314)
)

plan(sequential)

## ---- eval = requireNamespace("ggplot2", quietly = TRUE), fig.width = 6, fig.height = 2.5----
plot(res, fill = "darkred", alpha = 0.5)

