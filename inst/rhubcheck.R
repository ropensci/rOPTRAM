# R script to run checks on rhub (linux and windows)
# and macos check on macbuilds.org
t0 <- Sys.time()
print(paste(t0, " - Starting Check"))
cur_dir <- getwd()
devtools::check()
covr::package_coverage()

on.exit(setwd(cur_dir))
setwd("/home/docker")
tarball <- list.files(".", pattern="rOPTRAM.*tar.gz")
platforms <- c("windows-x86_64-release",
               "windows-x86_64-oldrel",
               "windows-x86_64-devel")

# Environment variable set in prepare_check.sh
env_vars <- c("OAUTH_CLIENTID" = Sys.getenv("OAUTH_CLIENTID"),
              "OAUTH_SECRET" = Sys.getenv("OAUTH_SECRET"))
rhub_chk <- rhub::check(path = tarball, platform = platforms,
                        show_status = TRUE, env_vars = env_vars)
statuses <- rhub_chk[[".__enclos_env__"]][["private"]][["status_"]]

res <- do.call(rbind, lapply(statuses, function(thisStatus) {
  data.frame(
    platform = thisStatus[["platform"]][["name"]],
    errors = length(thisStatus[["result"]][["errors"]]),
    warnings = length(thisStatus[["result"]][["warnings"]]),
    notes = length(thisStatus[["result"]][["notes"]]),
   stringsAsFactors = FALSE
  )
}))

# Check on MACOS
#pkgs <- c("dplyr", "ggplot2", "sf", "terra", "tools", "utils")
mac_url <- devtools::check_mac_release("./roptram")
Sys.sleep(600)
mac_res <- curl::curl(mac_url)
mac_res <- readLines(mac_res)
errs <- length(grep(pattern = "ERROR", mac_res))
warns <- length(grep(pattern = "WARNING", mac_res))
notes <- length(grep(pattern = "NOTE", mac_res))
mac_df <- data.frame("platform" = "macos_release",
                     "errors" = errs,
                     "warnings" = warns,
                     "notes" = notes)

res <- do.call(rbind, list(res, mac_df))
print(res, row.names = FALSE)

t1 <- Sys.time()
print(paste(t1, " - Check completed"))
duration <- as.numeric(difftime(t1, t0), units = "mins")
print(paste("Elapsed time for checking: ",
            sprintf("%.1f", duration),
            "minutes"))

if (any(colSums(res[2L:3L]) > 0)) {
  stop("Some checks with ERROR, or WARNING.")
}

if (colSums(res[4L]) > 0) {
  message("Some checks with NOTES.")
}
