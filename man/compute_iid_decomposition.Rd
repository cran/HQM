\name{compute_iid_decomposition}
\alias{compute_iid_decomposition}
\alias{compute_iid_decomposition_competing_risks}
\alias{compute_iid_decomposition_survival}
\title{IID decomposition for time-dependent AUC with IPCW}

\description{
  Internal function implementing the influence function (iid) decomposition 
  of the time-dependent Area Under the Curve (AUC) in the presence of right-censoring, 
  using inverse probability of censoring weighting (IPCW).
  
  This function reproduces the internal behavior of the corresponding routines 
  from the \code{timeROC} package. It supports both standard survival settings 
  and competing risks settings through automatic dispatch.
}

\usage{
  compute_iid_decomposition(t, n, cause, F01t, St,
                            weights, T, delta, marker, MatInt0TcidhatMCksurEff)
}

\arguments{
  \item{t}{
    Numeric scalar. Time point at which the AUC is evaluated.
  }
  \item{n}{
    Integer. Sample size.
  }
  \item{cause}{
    Integer or numeric. Event type of interest.
  }
  \item{F01t}{
    Numeric. Estimated cumulative incidence function at time \code{t}
    for the event of interest.
  }
  \item{St}{
    Numeric. Estimated survival probability at time \code{t}.
  }
  \item{weights}{
    List. IPCW weights object returned by \code{pec::ipcw}.
    Must contain components \code{IPCW.subjectTimes}, \code{IPCW.times},
    and \code{times}.
  }
  \item{T}{
    Numeric vector. Observed event or censoring times.
  }
  \item{delta}{
    Integer or numeric vector. Event indicator:
      \code{0} for censoring, \code{cause} for the event of interest,
    and other values for competing events.
  }
  \item{marker}{
    Numeric vector. Prognostic marker or risk score.
  }
  \item{MatInt0TcidhatMCksurEff}{
    Numeric matrix. Influence function representation of the censoring 
    distribution, typically obtained from \code{Compute.iid.KM()}.
  }
}

\details{
  The function computes the iid (influence function) representation of the 
  time-dependent AUC estimator based on IPCW methodology.
  
  Two cases are handled internally:
    
    \itemize{
      \item \strong{Survival setting:} only one event type is present 
      (no competing risks).
      \item \strong{Competing risks setting:} multiple event types are present,
      and the AUC is defined using different control definitions.
    }
  
  The implementation relies on pairwise comparisons between cases and controls,
  weighted by inverse probability of censoring weights. The iid representation 
  is derived from a U-statistic formulation and incorporates the influence of 
  the censoring distribution via martingale-based corrections.
  
  The computational complexity is quadratic in the sample size.
}

\value{
  A list with the following components:
    
    \item{iid_representation_AUC}{
      Numeric vector of length \code{n}. Influence function values for the AUC estimator.
    }
  
  \item{iid_representation_AUCstar}{
    Numeric vector of length \code{n}. Alternative influence function corresponding 
    to an alternative control definition (mainly relevant in competing risks).
  }
  
  \item{seAUC}{
    Numeric scalar. Estimated standard error of the AUC.
  }
  
  \item{seAUCstar}{
    Numeric scalar. Estimated standard error corresponding to \code{AUCstar}.
  }
  
  \item{computation_times}{
    Placeholder (numeric or \code{NA}). Included for compatibility with 
    \code{timeROC}.
  }
}

\seealso{
  \code{timeROC} (in the original \code{timeROC} package),
  \code{pec::ipcw},
  \code{\link{Compute.iid.KM}}
}

\references{
  Blanche, P., Dartigues, J. F., and Jacqmin-Gadda, H. (2013).
  Estimating and comparing time-dependent areas under receiver operating 
  characteristic curves for censored event times with competing risks.
  \emph{Statistics in Medicine}, 32(30), 5381–5397.
  
  Uno, H., Cai, T., Tian, L., and Wei, L. J. (2007).
  Evaluating prediction rules for t-year survivors with censored regression models.
  \emph{Journal of the American Statistical Association}, 102(478), 527–537.
}

\note{
  This is an internal utility function and is not intended for direct use 
  by end users. It is provided for reproducibility and to avoid dependency 
  on archived packages.
}

\author{
  Adapted from the \code{timeROC} package.
}

 