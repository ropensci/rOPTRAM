# R script to run checks on rhub (linux and windows)
# and macos check on macbuilds.org
t0 <- Sys.time()
print(paste(t0, " - Starting Check"))
setwd("/home/docker/rhub")

# Prepare for rhub to check on Ubuntu and Win
tarball <- list.files(".", pattern="rOPTRAM.*tar.gz")
platforms <- c("ubuntu-gcc-devel",
               "ubuntu-gcc-release",
               "windows-x86_64-release")

rhub_chk <- rhub::check(path = tarball, platform = platforms,
                        show_status = TRUE)
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
mac_url = devtools::check_mac_release()
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
