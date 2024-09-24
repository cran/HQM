"sn.0"<-function(xin, xout, h, kfun) { sapply(1:length(xout), function(i, xin, xout, h, kfun) sum(kfun((xin-xout[i])/h)), xin, xout, h, kfun) }

"sn.1"<-function(xin, xout, h, kfun) { sapply(1:length(xout), function(i, xin, xout, h, kfun)  sum(kfun((xin-xout[i])/h)*(xin-xout[i])), xin, xout, h, kfun) }

"sn.2"<-function(xin, xout, h, kfun) { sapply(1:length(xout), function(i, xin, xout, h, kfun) sum(kfun((xin-xout[i])/h)*((xin-xout[i])^2)), xin, xout, h, kfun) }
