# quick demo illustrating 
require(signal)
DELAY <- 64
bkrnd <- rnorm(2^14)
sig   <- chirp(t=1:2^10/2^8,f0=8,t1=4,f1=120,form="linear")
nzSD  <- 1
sigN  <- c(rnorm(2^11,sd=nzSD),
         sig*hanning(length(sig))+rnorm(length(sig),sd=nzSD),
         rnorm(2^11,sd=nzSD))
if(exists("opar")) par(opar)
specgram(sigN,n=64,Fs=8,overlap=16)
opar <-par(no.readonly =T)
nfft <- 2^11

dft1 <- fft(sigN[2000+(1:nfft)])[1:floor(nfft/2)]
sigN <- c(rnorm(2^11,sd=nzSD), # different random samples
         sig*hanning(length(sig))+rnorm(length(sig),sd=nzSD),
         rnorm(2^11,sd=nzSD))
dft2 <- fft(sigN[2000+DELAY+(1:nfft)])[1:floor(nfft/2)]
par(mfrow=c(2,1),mar=c(1.5,1.5,0,0),oma=c(3,3,3,3),mgp=c(1.7,.5,0))
plot(unwrap(Arg(dft1))-unwrap(Arg(dft2)),type="l",
     xlab="", ylab="")
abline(0,-2*pi*DELAY/nfft,col="red",lty=2)
title("Offsets reflect phase unwrapping discontinuities",outer=T)
plot(unwrap(Arg(dft1)-Arg(dft2)),type="l",xlab="",ylab="")
abline(0,-2*pi*DELAY/nfft,col="red",lty=2)
legend(x="top",legend=c("phase difference","theoretical"),
       col=c("black","red"),text.col=c("black","red"),lty=c(1,2))
mtext("frequency index",side=1,outer=T)
mtext("unwrapped phase difference",side=2,outer=T)

# two ways of estimating slope (both biased, but median less so)
# bias towards zero introduced because noise is identical in both
# signals, so the phase differences for the noise portion would be zero
# median will not work for a relatively narroband signal
print(c(median(diff(unwrap(Arg(dft1))-unwrap(Arg(dft2)))),
        -2*pi*DELAY/nfft))
print(c(median(diff(unwrap(Arg(dft1)-Arg(dft2)))),
        -2*pi*DELAY/nfft)) # more accurate
print(c(
  sum(Mod(dft1)[-1]^2*(diff(unwrap(Arg(dft1))-unwrap(Arg(dft2)))))/
    sum(Mod(dft1)^2),
  -2*pi*DELAY/nfft))
print(c(
  sum(Mod(dft1)[-1]^2*(diff(unwrap(Arg(dft1)-Arg(dft2)))))/
    sum(Mod(dft1)^2),
  -2*pi*DELAY/nfft))
