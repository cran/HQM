compute_iid_decomposition <- function(t, n, cause, F01t, St,
                                      weights, T, delta, marker,
                                      MatInt0TcidhatMCksurEff) {
  # Detect competing risks:
  has_competing <- any(delta != 0 & delta != cause)
  
  if (has_competing) {
    return(
      compute_iid_decomposition_competing_risks(
        t, n, cause, F01t, St,
        weights, T, delta, marker,
        MatInt0TcidhatMCksurEff
      )
    )
  } else {
    return(
      compute_iid_decomposition_survival(
        t, n, cause, F01t, St,
        weights, T, delta, marker,
        MatInt0TcidhatMCksurEff
      )
    )
  }
}


compute_iid_decomposition_competing_risks <- function(
    t, n, cause, F01t, St,
    weights, T, delta, marker,
    MatInt0TcidhatMCksurEff
) {
  
  Cases <- (T < t & delta == cause)
  Controls_1 <- (T > t)
  Controls_2 <- (T < t & delta != cause & delta != 0)
  
  which_Cases <- which(Cases)
  which_Controls_1 <- which(Controls_1)
  which_Controls_2 <- which(Controls_2)
  
  Weights_cases_all <- 1 / (weights$IPCW.subjectTimes * n)
  
  Weights_cases <- Weights_cases_all
  Weights_controls_2 <- Weights_cases_all
  
  Weights_cases[!Cases] <- 0
  Weights_controls_2[!Controls_2] <- 0
  
  Weights_controls_1 <- rep(
    1 / (weights$IPCW.times[which(weights$times == t)] * n),
    n
  )
  Weights_controls_1[!Controls_1] <- 0
  
  nb_Cases <- length(which_Cases)
  nb_Controls_1 <- length(which_Controls_1)
  nb_Controls_2 <- length(which_Controls_2)
  
  htij1 <- function(V) {
    (V[,1] > t) *
      ((V[,4] > V[,2]) + 0.5 * (V[,4] == V[,2])) *
      (V[,3] * V[,5]) * (n * n)
  }
  
  htij2 <- function(V) {
    (V[,1] <= t) *
      ((V[,4] > V[,2]) + 0.5 * (V[,4] == V[,2])) *
      (V[,6] != 0) * (V[,6] != cause) *
      (V[,3] * V[,5]) * (n * n)
  }
  
  Mathtij1 <- matrix(0, nb_Controls_1, nb_Cases)
  Mathtij2 <- matrix(0, nb_Controls_2, nb_Cases)
  
  for (k in seq_along(which_Cases)) {
    i <- which_Cases[k]
    
    if (nb_Controls_1 > 0) {
      W1 <- cbind(
        T[which_Controls_1],
        marker[which_Controls_1],
        rep(Weights_cases[i], nb_Controls_1),
        rep(marker[i], nb_Controls_1),
        Weights_controls_1[which_Controls_1]
      )
      Mathtij1[, k] <- htij1(W1)
    }
    
    if (nb_Controls_2 > 0) {
      W2 <- cbind(
        T[which_Controls_2],
        marker[which_Controls_2],
        rep(Weights_cases[i], nb_Controls_2),
        rep(marker[i], nb_Controls_2),
        Weights_controls_2[which_Controls_2],
        delta[which_Controls_2]
      )
      Mathtij2[, k] <- htij2(W2)
    }
  }
  
  ht <- (sum(Mathtij1) + sum(Mathtij2)) / (n * n)
  
  sum_ijk <- function(l) {
    term1 <- if (nb_Controls_1 > 0)
      sum(Mathtij1 * (1 + MatInt0TcidhatMCksurEff[which_Controls_1, l]))
    else 0
    
    term2 <- if (nb_Controls_2 > 0)
      sum(Mathtij2 * (1 + MatInt0TcidhatMCksurEff[which_Controls_2, l]))
    else 0
    
    term1 + term2 - n^2 * ht
  }
  
  iid <- sapply(1:n, sum_ijk) / (n^2 * F01t * (1 - F01t))
  
  se <- sqrt(var(iid) / n)
  
  list(
    iid_representation_AUC = iid,
    iid_representation_AUCstar = iid,  # placeholder for compatibility
    seAUC = se,
    seAUCstar = se,
    computation_times = NA
  )
}


compute_iid_decomposition_survival <- function(
    t, n, cause, F01t, St,
    weights, T, delta, marker,
    MatInt0TcidhatMCksurEff
) {
  
  Cases <- (T < t & delta == cause)
  Controls <- (T > t)
  
  which_Cases <- which(Cases)
  which_Controls <- which(Controls)
  
  Weights_cases_all <- 1 / (weights$IPCW.subjectTimes * n)
  
  Weights_cases <- Weights_cases_all
  Weights_controls <- rep(
    1 / (weights$IPCW.times[which(weights$times == t)] * n),
    n
  )
  
  Weights_cases[!Cases] <- 0
  Weights_controls[!Controls] <- 0
  
  nb_Cases <- length(which_Cases)
  nb_Controls <- length(which_Controls)
  
  htij <- function(V) {
    (V[,1] > t) *
      ((V[,4] > V[,2]) + 0.5 * (V[,4] == V[,2])) *
      (V[,3] * V[,5]) * (n * n)
  }
  
  Mathtij <- matrix(0, nb_Controls, nb_Cases)
  
  for (k in seq_along(which_Cases)) {
    i <- which_Cases[k]
    
    if (nb_Controls > 0) {
      W <- cbind(
        T[which_Controls],
        marker[which_Controls],
        rep(Weights_cases[i], nb_Controls),
        rep(marker[i], nb_Controls),
        Weights_controls[which_Controls]
      )
      Mathtij[, k] <- htij(W)
    }
  }
  
  ht <- sum(Mathtij) / (n * n)
  
  sum_ijk <- function(l) {
    sum(Mathtij * (1 + MatInt0TcidhatMCksurEff[which_Controls, l])) - n^2 * ht
  }
  
  iid <- sapply(1:n, sum_ijk) / (n^2 * F01t * (1 - F01t))
  
  se <- sqrt(var(iid) / n)
  
  list(
    iid_representation_AUC = iid,
    iid_representation_AUCstar = iid,
    seAUC = se,
    seAUCstar = se,
    computation_times = NA
  )
}