# ---- Tier 5 internal helpers --------------------------------------------

# Plain 2SLS: regress y on X, instrumenting with Z (Z must span X's
# exogenous columns).  Returns coefficients, HC0-style SEs, t values.
.cdr_tsls <- function(y, X, Z) {
  X <- as.matrix(X); Z <- as.matrix(Z); y <- as.numeric(y)
  pz    <- Z %*% solve(crossprod(Z), t(Z))
  xhat  <- pz %*% X
  xtxi  <- solve(crossprod(xhat))
  beta  <- as.vector(xtxi %*% crossprod(xhat, y))
  names(beta) <- colnames(X)
  resid <- as.vector(y - X %*% beta)
  n <- nrow(X); k <- ncol(X)
  vc <- sum(resid^2) / (n - k) * xtxi
  se <- sqrt(diag(vc))
  list(coef = beta, se = se, t = beta / se, resid = resid, n = n)
}

# Row-standardized k-nearest-neighbour weights matrix in the space spanned
# by `coords` (a numeric matrix, one row per unit).
.cdr_knn_weights <- function(coords, k = 5L) {
  coords <- as.matrix(coords)
  n <- nrow(coords)
  k <- min(k, n - 1L)
  d <- as.matrix(stats::dist(coords))
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    nb <- order(d[i, ])[2:(k + 1L)]      # skip self (distance 0)
    W[i, nb] <- 1
  }
  rs <- rowSums(W)
  rs[rs == 0] <- 1
  W / rs
}

# Moran's I for `x` under row-standardized weights `W`, with an analytic
# normal p-value and an optional permutation p-value.
.cdr_moran <- function(x, W, n_perm = 999L) {
  x  <- x - mean(x)
  n  <- length(x)
  S0 <- sum(W)
  num <- as.numeric(t(x) %*% W %*% x)
  I   <- (n / S0) * num / sum(x^2)
  eI  <- -1 / (n - 1)

  # analytic variance (Cliff-Ord, normality assumption)
  S1 <- 0.5 * sum((W + t(W))^2)
  S2 <- sum((rowSums(W) + colSums(W))^2)
  vI <- (n^2 * S1 - n * S2 + 3 * S0^2) /
        ((n^2 - 1) * S0^2) - eI^2
  z  <- (I - eI) / sqrt(vI)
  p_norm <- 2 * stats::pnorm(-abs(z))

  p_perm <- NA_real_
  if (n_perm > 0) {
    sim <- replicate(n_perm, {
      xp <- sample(x)
      (n / S0) * as.numeric(t(xp) %*% W %*% xp) / sum(xp^2)
    })
    p_perm <- (1 + sum(abs(sim - eI) >= abs(I - eI))) / (n_perm + 1)
  }
  list(I = I, expectation = eI, sd = sqrt(vI), z = z,
       p_norm = p_norm, p_perm = p_perm)
}
