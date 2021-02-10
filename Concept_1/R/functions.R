
custom_datatable <- function(...){
  # wrapper around DT::datatable so commonly used arguments
    # can be set as global defaults
  
  DT::datatable(..., rownames = FALSE, 
                options = list(
                  # sets n observations shown
                  pageLength = 20,
                  # removes option to change n observations shown
                  lengthChange = FALSE,
                  # removes the search bar
                  sDom  = '<"top">lrt<"bottom">ip',
                  # enable side scroll so table doesn't overflow
                  scrollX = TRUE
                )
  )
}

# example data set for demo 
make_dat_demo <- function(n = 400){
  inv.log <- function(x){exp(x*.8)/(1 + exp(x*.8))}
  X <- runif(n, min = 0,max = 2)
  X2 <- rnorm(n, -2, 5)
  prob <- inv.log((-2 + (X^2)))
  Z <- rbinom(n, 1, prob)
  A <- matrix(runif(4), 2, 2)
  A <- A %*% t(A)
  diag(A) <- 1
  Sigma <- A
  X3 <- MASS::mvrnorm(n, rep(2, 2), Sigma)[, 1]
  X4 <- MASS::mvrnorm(n, rep(2, 2), Sigma)[, 2]
  Y0 <- 2*X + .8*X2 + rnorm(n, 0, 1)
  Y1 <- 2*X + 3*(X^2) + .8*X2 +  rnorm(n, 0, 1)
  df <- data.frame(X, prob, Z, Y0, Y1, X2, X3, X4)
  df <- df %>% mutate(Y = if_else(Z ==1, Y1, Y0))
  df <- df %>% dplyr::select(-c(Y0, Y1, prob))
  return(df)
}