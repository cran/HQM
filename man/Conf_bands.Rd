\name{Conf_bands}
\title{Confidence bands}
\alias{Conf_bands}
\description{
Implements the uniform and pointwise confidence bands for the future conditional hazard rate based on the last observed marker measure.
}

\usage{
Conf_bands(data,   marker_name, event_time_name = 'years',
            time_name = 'year', event_name = 'status2', x, b)
}

\arguments{
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{marker_name}{The column name of the marker values in the data frame \code{\link{data}}. }
\item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
\item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
\item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
\item{x}{Numeric value of the last observed marker value.}
\item{b}{Bandwidth.}
}

\details{
The function \code{\link{Conf_bands}} implements the pointwise and uniform confidence bands for the estimator of the future conditional hazard rate \eqn{\hat h_x(t)}. The confidence bands are based on a wild bootstrap approach \eqn{{h^*}_{{x_*},B}(t)}.

Pointwise:
For a given \eqn{t\in (0,T)} generate \eqn{{h^*}_{{x_*},B}^{(1)}(t),...,{h^*}_{{x_*},B}^{(N)}(t)} for \eqn{N = 1000} and order it \eqn{{h^*}_{{x_*},B}^{[1]}(t)\leq ...\leq {h^*}_{{x_*},B}^{[N]}(t)}. Then
\deqn{
\hat{I}^1_{n,N} = \Bigg[\hat{h}_{x_*}(t) - \hat{\sigma}_{{G}_{x_*}}(t)\frac{{h^*}_{{x_*},B}^{[ N(1-\frac{\alpha}{2})]}(t)}{\sqrt{n}}, \hat{h}_{x_*}(t) -  \hat{\sigma}_{ {G}_x}(t)\frac{{h^*}_{{x_*},B}^{[ N\frac{\alpha}{2}]}(t)}{\sqrt{n}}\Bigg]
}
is a \eqn{1-\alpha} pointwise confidence band for \eqn{h_{x_*}(t)}, where \eqn{\hat{\sigma}_{{G}_{x_*}}(t)} is a bootrap estimate of the variance. For more details on the wild bootstrap approach, please see \code{\link{prep_boot}} and \code{\link{g_xt}}.

Uniform:
Generate \eqn{\bar{h}_{{x_*},B}^{(1)}(t),...,\bar{h}_{{x_*},B}^{(N)}(t)} for \eqn{N = 1000} for all \eqn{t\in [\delta_T,T-\delta_T]} and define \eqn{W^{(i)} = \sup_{t\in[0,T]}\big|\bar{h}_{{x_*},B}^{(i)}(t)|} for \eqn{i = 1,...,N}. Order \eqn{W^{[1]} \leq ... \leq W^{[N]}}. Then
\deqn{\hat{I}^2_{n,N} = \Bigg[\hat{h}_{x_*}(t) \pm \hat{\sigma}_{{G}_{x_*}}(t) \frac{W^{[ N(1 - \alpha)]}}{\sqrt{n}} \Bigg]}
is a \eqn{1-\alpha} uniform confidence band for \eqn{h_{x_*}(t)}.
}

\value{
A list with pointwise, uniform confidence bands and the estimator \eqn{\hat h_x(t)} for all possible time points \eqn{t}.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{g_xt}, \link{prep_boot} }}

\examples{
b = 10
x = 3
size_s_grid<-100
s = pbc2$year
br_s = seq(0, max(s), max(s)/( size_s_grid-1))


c_bands = Conf_bands(pbc2,   'serBilir', event_time_name = 'years',
                    time_name = 'year', event_name = 'status2', x, b)

J = 80
plot(br_s[1:J], c_bands$h_hat[1:J], type = "l", ylim = c(0,1), ylab = 'Hazard', xlab = 'Years')

lines(br_s[1:J], c_bands$I_p_up[1:J], col = "red")
lines(br_s[1:J], c_bands$I_p_do[1:J], col = "red")
lines(br_s[1:J], c_bands$I_nu[1:J], col = "blue")
lines(br_s[1:J], c_bands$I_nd[1:J], col = "blue")
}
