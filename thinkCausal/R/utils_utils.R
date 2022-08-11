`%notin%` <- Negate(`%in%`)

coerce_to_logical <- function(x){
  out <- NA
  out[x %in% c(1, '1', TRUE, T, 'TRUE', 'T', 'true', 't')] <- TRUE
  out[x %in% c(0, '0', FALSE, F, 'FALSE', 'F', 'false', 'f')] <- FALSE
  return(out)
}

is_categorical <- function(x){
  is_int <- isTRUE(clean_detect_integers(x))
  is_binary <- isTRUE(clean_detect_logical(x))
  is_cont <- isTRUE(is.double(x))
  is_cat <- !any(is_int, is_binary, is_cont)
  return(is_cat)
}

is_cat_or_logical <- function(x){
  is_log <- isTRUE(clean_detect_logical(x))
  is_cat <- isTRUE(is_categorical(x))
  is_either <- is_log | is_cat
  return(is_either)
}

#' Force renv lock file to use CRAN
#'
#' Identifies packages in renv.lock that do not use CRAN as their repo. Removes, re-downloads, and re-installs this packages from CRAN. Then it updates the lock file.
#'
#' @return NULL
#' @author Joe Marlo
#' @noRd
renv_force_all_cran_repo <- function(){
  get_non_cran <- function(){
    lock <- jsonlite::read_json('renv.lock')
    non_cran_packages <- purrr::map_lgl(lock$Packages, function(pkg) pkg$Repository != 'CRAN')
    non_cran_packages <- names(non_cran_packages[non_cran_packages])
    return(non_cran_packages)
  }

  # remove and reinstall packages
  non_cran_packages <- get_non_cran()
  if (length(non_cran_packages) != 0){
    cli::cli_alert_info('Trying to update these packages: {non_cran_packages}')
    remove.packages(non_cran_packages)
    renv::snapshot(prompt = FALSE)
    install.packages(non_cran_packages, force = TRUE, repos = "https://cloud.r-project.org")
    renv::snapshot(prompt = FALSE)

    # check to see there are non leftover
    non_cran_packages_2 <- get_non_cran()
    if (length(non_cran_packages_2) != 0) cli::cli_alert_warning('These packages could not be fixed: {non_cran_packages_2}')
  } else {
    cli::cli_alert_success('No changes: All packages use CRAN as their repo')
  }

  return(invisible(NULL))
}
