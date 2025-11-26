# Weighted KS test function


# function for weighted ks
ecdf_w <- function(vals, weights) {
  ord <- order(vals)
  vals <- vals[ord]
  weights <- weights[ord]
  cumw <- cumsum(weights)
  stepfun(vals, c(0, cumw))
}

weighted_ks_test <- function(x, y, w_x = NULL, w_y = NULL, B = 1000) {
  # Default weights = equal
  if (is.null(w_x)) w_x <- rep(1, length(x))
  if (is.null(w_y)) w_y <- rep(1, length(y))
  
  # Normalize weights
  w_x <- w_x / sum(w_x)
  w_y <- w_y / sum(w_y)
  
  # Weighted ECDFs
  F_x <- ecdf_w(x, w_x)
  F_y <- ecdf_w(y, w_y)
  
  # KS statistic
  all_points <- sort(unique(c(x, y)))
  D_obs <- max(abs(F_x(all_points) - F_y(all_points)))
  
  # Bootstrap p-value (permutation test)
  pooled <- c(x, y)
  weights <- c(w_x, w_y)
  n_x <- length(x)
  
  D_perm <- replicate(B, {
    perm <- sample(length(pooled))
    x_perm <- pooled[perm[1:n_x]]
    y_perm <- pooled[perm[(n_x+1):length(pooled)]]
    w_xp <- weights[perm[1:n_x]]
    w_yp <- weights[perm[(n_x+1):length(pooled)]]
    
    F_xp <- ecdf_w(x_perm, w_xp)
    F_yp <- ecdf_w(y_perm, w_yp)
    max(abs(F_xp(all_points) - F_yp(all_points)))
  })
  
  p_val <- mean(D_perm >= D_obs)
  
  list(statistic = D_obs, p.value = p_val, method = "Weighted KS test", B = B)
}
