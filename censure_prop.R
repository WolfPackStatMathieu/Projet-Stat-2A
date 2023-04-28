# Proportion censure Alt
# 
source("analyse_alt/travail_comp.R")
source("generation_echantillon/generation_ech_comp.R")
source("http://myweb.uiowa.edu/pbreheny/7210/f19/notes/fun.R")

prop_censure_alt <- function(N,p_cause1,n,type1,type2,t_star,graine=133){
  
  p <- 33
  p_cause2<-(1-p_cause1)
  
  res <- list()
  censures <- rep(NA,N)
  TDL <- rep(NA,N)
  gueris <- rep(NA,N)
  for(i in 1:N){
    
    df <- generation_comp(p_cause1=p_cause1, p_cause2=p_cause2, 
                        t_star=t_star,nombre_obs=n,
                        type1=type1,type2=type2,graine=graine+i)
  
    res[[i]] <- df
    nb_status0 <- length(which(df$status==0))
    nb_status1 <- length(which(df$status==1))
    nb_status2 <- length(which(df$status==2))
    
    censures[i] <- (nb_status0/n)*100     # censures
    TDL[i] <- (nb_status1/n)*100    # non guéris (toxicité)
    gueris[i] <- (nb_status2/n)*100    # guéris
  }
  # censures_mean <- mean(censures)
  # TDL_mean <- mean(TDL)
  # gueris_mean <- mean(gueris)
  
  dens_censure <- density(censures)
  dens_TDL <- density(TDL)
  
  par(mfrow = c(1,2))

  plot(x=dens_TDL$x, y=dens_TDL$y, main="TDL", type="l",xlab="TDL", ylab="densité")
  abline(v=p, col="red")
  # Add legend
  legend("topright", # Position of the legend
         "cible TDL", # Labels
         col = "red",
         pch = c(19, NA), # Point markers (NA means no marker)
         lty = c(1, 1), # Line styles (1 means solid)
         lwd = c(2, 2),
         bty ="n",
         cex = 0.6) # Line widths
  plot(x=dens_censure$x, y=dens_censure$y, main="Censures", type="l", xlab="censure", ylab="densité")
  # M <- cbind(TDL, censures)
  # dnplot(M[,1])
  # dnplot(M[,1], pos=TRUE)
  # dnplot(M)
  # dnplot(M, labs=c('TDL', 'Censures'))
  # dnplot(M, labs=c('TDL', 'Censures'))
  
}

# Test
p_cause1<-0.33
n<-25
type1<-"constant"
type2<-"constant"
t_star<-6

prop_censure_alt(1900,p_cause1=p_cause1, n=n, type1, type2, t_star=t_star, graine=133)

