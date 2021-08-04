#---------------------------------------------------------------------------------------------------------------
# Basic Settings // Data Loading
#---------------------------------------------------------------------------------------------------------------
#setwd("~/Documents/BACKUP/MODEL1/RUN1")
setwd("~/Dropbox/SHARED/MJEON2/ANALYSIS/GEPS/MODEL1/RUN1")
library(kknn);
library(SNFtool);
library(MASS);
library(flexclust);
nitem = 72;
nschool = 62;
ndyad = choose(nitem, 2);
ndim = 2;
ncat = 1;
nmax = 81;
niter = 2500;
count = scan("DATA/count.txt")
option = 1

# Load Dataset and Make Adjacency Matrices
itemsum = matrix(NA,nschool,nitem)
sampsum = matrix(NA,nschool,nmax)
Y = array(0, dim=c(nschool,nitem,nmax,nmax))
U = array(0, dim=c(nschool,nmax,nitem,nitem))
for(a in 1:nschool){
  if(a < 10){
    vname = paste("item0",a,sep="")
    fname = paste("DATA/item0",a,".txt",sep="")
  }
  else{
    vname = paste("item",a,sep="")
    fname = paste("DATA/item",a,".txt",sep="")
  }
  temp = matrix(scan(fname),ncol=nitem,byrow=TRUE)
  assign(vname,temp)
  
  for(k in 1:nitem){
    for(i in 2:count[a]){
      for(j in 1:(i-1)){
        Y[a,k,i,j] = Y[a,k,j,i] = temp[i,k] * temp[j,k]
      }
    }
  }
  for(k in 1:count[a]){
    for(i in 2:nitem){
      for(j in 1:(i-1)){
        U[a,k,i,j] = U[a,k,j,i] = temp[k,i] * temp[k,j]
      }
    }
  }
  itemsum[a,] = colSums(temp)
  sampsum[a,(1:count[a])] = rowSums(temp)
}

sampvec = rep(NA,sum(count)); index = 0;
for(i in 1:nschool){
  sampvec[(index+1):(index+count[i])] = sampsum[i,(1:count[i])]
  index = index + count[i]
}

#---------------------------------------------------------------------------------------------------------------
# Trace Plot for Beta, Theta, Sigma, Respondent and Item Latent Spaces (School Level)
#---------------------------------------------------------------------------------------------------------------
sim_beta  = array(NA,dim=c(nschool,niter,nitem))
sim_theta = array(NA,dim=c(nschool,niter,nmax))
sim_sigz  = matrix(NA,niter,nschool)
sim_z = array(NA,dim=c(nschool,niter,nmax*ndim))
sim_i = array(NA,dim=c(nschool,niter,nitem*ndim))
theta_result = rep(NA,sum(count))
a = 1:2500; index = 0;
for(i in 1:nschool){
  if(i < 10){
    fname1 = paste("RESULT/sim0",i,"_b1.log",sep="")
    fname2 = paste("RESULT/sim0",i,"_t1.log",sep="")
    fname3 = paste("RESULT/sim0",i,"_h1.log",sep="")
    fname4 = paste("RESULT/sim0",i,"_z1.log",sep="")
    fname5 = paste("RESULT/sim0",i,"_i1.log",sep="")
    oname1 = paste("TRACE/beta0",i,".pdf",sep="")
    oname2 = paste("TRACE/theta0",i,".pdf",sep="")
    oname3 = paste("TRACE/resp0",i,".pdf",sep="")
    oname4 = paste("TRACE/item0",i,".pdf",sep="")
  }
  else{
    fname1 = paste("RESULT/sim",i,"_b1.log",sep="")
    fname2 = paste("RESULT/sim",i,"_t1.log",sep="")
    fname3 = paste("RESULT/sim",i,"_h1.log",sep="")
    fname4 = paste("RESULT/sim",i,"_z1.log",sep="")
    fname5 = paste("RESULT/sim",i,"_i1.log",sep="")
    oname1 = paste("TRACE/beta",i,".pdf",sep="")
    oname2 = paste("TRACE/theta",i,".pdf",sep="")
    oname3 = paste("TRACE/resp",i,".pdf",sep="")
    oname4 = paste("TRACE/item",i,".pdf",sep="")
  }
  temp = matrix(scan(fname1),ncol=nitem,byrow=TRUE)
  if(option == 1) pdf(oname1); for(j in 1:nitem) plot(a,temp[,j],type="l",main=i); dev.off();
  sim_beta[i,,] = temp
  temp = matrix(scan(fname2),ncol=count[i],byrow=TRUE)
  if(option == 1) pdf(oname2); for(j in 1:count[i]) plot(a,temp[,j],type="l",main=i); dev.off();
  sim_theta[i,,1:count[i]] = temp
  theta_result[(index+1):(index+count[i])] = apply(temp,2,mean)
  index = index + count[i]
  temp = scan(fname3)
  sim_sigz[,i] = temp
  temp = matrix(scan(fname4),ncol=count[i]*ndim,byrow=TRUE)
  sim_z[i,,1:(count[i]*ndim)] = temp
  if(option == 1) pdf(oname3); for(j in 1:(count[i]*ndim)) plot(a,temp[,j],type="l",main=i); dev.off();
  temp = matrix(scan(fname5),ncol=nitem*ndim,byrow=TRUE)
  sim_i[i,,1:(nitem*ndim)] = temp
  if(option == 1) pdf(oname4); for(j in 1:(nitem*ndim)) plot(a,temp[,j],type="l",main=i); dev.off();
}
pdf("TRACE/sigz.pdf"); for(i in 1:nschool) plot(a,sim_sigz[,i],type="l",main=i); dev.off();

theta_mean = matrix(NA,nitem+1,5)
for(i in 26:66){
  temp = theta_result[sampvec==i]
  theta_mean[(i+1),] = c(min(temp),quantile(temp,c(0.25,0.5,0.75)),max(temp))
}
x.axis = c(26:66)
y.axis = x.axis
pdf("PLOT/hpd_theta.pdf")
plot(x.axis,y.axis,ylim=c(-1.0,4.00),xlab="total score",ylab=expression(theta),type="n")
for(i in 26:66){
  lines(c(i,i),c(theta_mean[i,1],theta_mean[i,5]),lwd=2)
  points(i,theta_mean[i,3],pch=20,col=4)
} 
for(i in 3:6) abline(v=i*10, lty=3, col=2)
dev.off()

#---------------------------------------------------------------------------------------------------------------
# Trace Plot for Hyperparameter
# Sigma: Variances of Random Effects in Item Dependence Matrices
# Delta: Fixed Effects in Item Dependence Matrices
# Tau: Variances of Fixed Effects in Item Dependence Matrices
# Gamma: Hyperparameter for Beta
# Varphi: Variance of Hyperpameter for Beta
# Sdist: School Distances Calculated from Item Dependence Matrices
#---------------------------------------------------------------------------------------------------------------
sim_sigma  = matrix(scan("RESULT/sim_s1.log"),ncol=nschool,byrow=TRUE)
sim_delta  = matrix(scan("RESULT/sim_l1.log"),ncol=ndyad*ncat,byrow=TRUE)
sim_tau    = matrix(scan("RESULT/sim_u1.log"),ncol=ndyad*ncat,byrow=TRUE)
sim_gamma  = matrix(scan("RESULT/sim_g1.log"),ncol=nitem,byrow=TRUE)
sim_varphi = matrix(scan("RESULT/sim_p1.log"),ncol=nitem,byrow=TRUE)
sim_sdist  = matrix(scan("RESULT/sim_a1.log"),ncol=choose(nschool,2),byrow=TRUE)
if(option==1){
  pdf("TRACE/sigma.pdf");  for(i in 1:nschool) plot(a,sim_sigma[,i],type="l",main=i); dev.off();
  pdf("TRACE/delta.pdf");  for(i in 1:(ndyad*ncat)) plot(a,sim_delta[,i],type="l",main=i); dev.off();
  pdf("TRACE/tau.pdf");    for(i in 1:(ndyad*ncat)) plot(a,sim_tau[,i],  type="l",main=i); dev.off();
  pdf("TRACE/gamma.pdf");  for(i in 1:nitem) plot(a,sim_gamma[,i], type="l",main=i); dev.off();
  pdf("TRACE/varphi.pdf"); for(i in 1:nitem) plot(a,sim_varphi[,i], type="l",main=i); dev.off();
  pdf("TRACE/school.pdf");  for(i in 1:choose(nschool,2)) plot(a,sim_sdist[,i],type="l"); dev.off();
}

#---------------------------------------------------------------------------------------------------------------
# Mean and 95% HPD Intervals 
# Gamma (The Hyperparameter for Beta)
# Varphi (The Variance of Hyperparameter for Beta)
# Variance for Latent Spaces
#---------------------------------------------------------------------------------------------------------------
library(coda)
# 1. Gamma (The Hyperparameter for Beta)
gamma_mean = apply(sim_gamma,2,mean)
gamma_mcmc = mcmc(sim_gamma)
gamma_result = matrix(NA,length(gamma_mean),3)
gamma_result[,1] = gamma_mean
gamma_result[,2:3] = HPDinterval(gamma_mcmc,prob=0.95)
round(gamma_result,4)
min(gamma_result)
max(gamma_result)
pdf("PLOT/hpd_gamma.pdf")
x.axis = c(1:nitem)
y.axis = x.axis
plot(x.axis,y.axis,ylim=c(-3.75,5.50),xlab="item",ylab=expression(gamma),type="n")
for(i in 1:nitem){
  lines(c(i,i),gamma_result[i,2:3],lwd=2)
  points(i,gamma_result[i,1],pch=20,col=4)
} 
for(i in 1:7) abline(v=i*10, lty=3, col=2)
dev.off()

# 2. Varphi (The Hyper-Variance Parameter for Beta)
varphi_mean = apply(sim_varphi,2,mean)
varphi_mcmc = mcmc(sim_varphi)
varphi_result = matrix(NA,length(varphi_mean),3)
varphi_result[,1] = varphi_mean
varphi_result[,2:3] = HPDinterval(varphi_mcmc,prob=0.95)
round(varphi_result,4)
min(varphi_result)
max(varphi_result)
pdf("PLOT/hpd_varphi.pdf")
x.axis = c(1:nitem)
y.axis = x.axis
plot(x.axis,y.axis,ylim=c(0.25,2.00),xlab="item",ylab=expression(sigma[beta]^2),type="n")
for(i in 1:nitem){
  lines(c(i,i),varphi_result[i,2:3],lwd=2)
  points(i,varphi_result[i,1],pch=20,col=4)
} 
for(i in 1:7) abline(v=i*10, lty=3, col=2)
dev.off()

# 3. Variance for Latent Spaces (Annotate for Fixed Variance)
sigz_mean = apply(sim_sigz,2,mean)
sigz_mcmc = mcmc(sim_sigz)
sigz_result = matrix(NA,length(sigz_mean),3)
sigz_result[,1] = sigz_mean
sigz_result[,2:3] = HPDinterval(sigz_mcmc,prob=0.95)
round(sigz_result,4)
min(sigz_result)
max(sigz_result)
pdf("PLOT/hpd_sigmaz.pdf")
x.axis = c(1:nschool)
y.axis = x.axis
plot(x.axis,y.axis,ylim=c(0.00,1.00),xlab="school",ylab=expression(sigma[z]^2),type="n")
for(i in 1:nschool){
  lines(c(i,i),sigz_result[i,2:3],lwd=2)
  points(i,sigz_result[i,1],pch=20,col=4)
} 
for(i in 1:6) abline(v=i*10, lty=3, col=2)
dev.off()

# 4. Variance 
sigma_mean = apply(sim_sigma,2,mean)
sigma_mcmc = mcmc(sim_sigma)
sigma_result = matrix(NA,length(sigma_mean),3)
sigma_result[,1] = sigma_mean
sigma_result[,2:3] = HPDinterval(sigma_mcmc,prob=0.95)
round(sigma_result,4)
min(sigma_result)
max(sigma_result)
pdf("PLOT/hpd_sigma.pdf")
x.axis = c(1:nschool)
y.axis = x.axis
plot(x.axis,y.axis,ylim=c(0.0,1.0),xlab="school",ylab=expression(sigma[d]^2),type="n")
for(i in 1:nschool){
  lines(c(i,i),sigma_result[i,2:3],lwd=2)
  points(i,sigma_result[i,1],pch=20,col=4)
} 
for(i in 1:6) abline(v=i*10, lty=3, col=2)
dev.off()

#---------------------------------------------------------------------------------------------------------------
# Beta and Theta Estimates Summary
#---------------------------------------------------------------------------------------------------------------
betasum = matrix(NA,nschool,nitem)
thetasum = matrix(NA,nschool,nmax)
for(i in 1:nschool){
  if(i < 10){
    fname1 = paste("RESULT/sum0",i,"_b1.log",sep="")
    fname2 = paste("RESULT/sum0",i,"_t1.log",sep="")
  }
  else{
    fname1 = paste("RESULT/sum",i,"_b1.log",sep="")
    fname2 = paste("RESULT/sum",i,"_t1.log",sep="")
  }
  temp = matrix(scan(fname1),ncol=nitem,byrow=TRUE)
  betasum[i,] = temp[1,]
  temp = matrix(scan(fname2),ncol=count[i],byrow=TRUE)
  thetasum[i,1:count[i]] = temp[1,]
}

write.table(itemsum,"RSUM/itemsum.txt",row.names=FALSE,col.names=FALSE)
write.table(sampsum,"RSUM/sampsum.txt",row.names=FALSE,col.names=FALSE)
write.table(betasum,"RSUM/betasum.txt",row.names=FALSE,col.names=FALSE)
write.table(thetasum,"RSUM/thetasum.txt",row.names=FALSE,col.names=FALSE)

#---------------------------------------------------------------------------------------------------------------
# Draw HeatMap Using Combined Estimated Item Distance Matrices
# Recover Positions from Combined Estimated Item Distance Matrices Using Nonparametric Multidimensional Scaling
#---------------------------------------------------------------------------------------------------------------
temp = matrix(scan("RESULT/sum_l1.log"),ncol=ndyad,byrow=TRUE)
temp1 = temp[1,1:ndyad];
idist = matrix(0,nitem,nitem);
logidist = matrix(0,nitem,nitem);
index = 0;
for(i in 2:nitem){
  for(j in 1:(i-1)){
    index = index + 1
    idist[i,j] = exp(temp1[index])
    logidist[i,j] = temp1[index]
  }
}
idist = idist + t(idist)
logidist = logidist + t(logidist)

pdf("PLOT/headmap.pdf");
heatmap(idist, symm=TRUE, keep.dendro=FALSE, cexRow=0.5, cexCol=0.5);
dev.off();

if(option == 1){
  ntrad = isoMDS(idist,k=2)
  #ntrad = cmdscale(idist,eig=TRUE,k=2)
  ntrad_x = ntrad$points[,1]; ntrad_y = ntrad$points[,2];
  write.table(ntrad$points,"RSUM/position.txt",row.names=FALSE,col.names=FALSE)
}else{
  temp = matrix(scan("RSUM/position.txt"),ncol=2,byrow=TRUE)
  ntrad_x = temp[,1]; ntrad_y = temp[,2];
}

pdf("PLOT/mu_xy.pdf")
plot(ntrad_x, ntrad_y, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, cex=.7)
dev.off()

#---------------------------------------------------------------------------------------------------------------
# Spectral Clustering for Combined Estimated Item Distance Matrices
#---------------------------------------------------------------------------------------------------------------

# 1. Spectral Clustering with K-means
W = affinityMatrix(idist,K=round((nitem-1)/2,0))
#nclust = estimateNumberOfClustersGivenGraph(W,NUMC=2:14)
ncluster = 3
if(option==1){
  group = spectralClustering(W,ncluster);
  write.table(group,"RSUM/group.txt",row.names=FALSE,col.names=FALSE)
}else{
  group = scan("RSUM/group.txt")
}
table(group)

# 2. Spectral Clustering with K-medians
d = rowSums(W)
d[d == 0] = .Machine$double.eps
D = diag(d)
L = D - W
Di = diag(1 / sqrt(d))
NL = Di %*% L %*% Di
eig = eigen(NL)
res = sort(abs(eig$values),index.return = TRUE)
U = eig$vectors[,res$ix[1:ncluster]]
normalize <- function(x) x / sqrt(sum(x^2))
U = t(apply(U,1,normalize))
item_label = kcca(U, ncluster, family=kccaFamily("kmedians"))
if(option==1){
  itemlabel = item_label@cluster
  write.table(itemlabel,"RSUM/group_med.txt",row.names=FALSE,col.names=FALSE)
}else{
  itemlabel = scan("RSUM/group_med.txt")
}
group[group==1] = 4
group[group==3] = 1

# 3. Plot Spectral Clustering (with K-median) Results
pdf("PLOT/mu_spec_mean.pdf")
plot(ntrad_x, ntrad_y, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, col=group, cex=.7)
dev.off()
pdf("PLOT/mu_spec_median.pdf")
plot(ntrad_x, ntrad_y, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, col=itemlabel, cex=.7)
dev.off()

pdf("PLOT/mu_spec_mean_en1.pdf")
plot(ntrad_x, ntrad_y, xlim=c(0.0,0.5), ylim=c(-0.25,0.25), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, col=group, cex=.7)
dev.off()
pdf("PLOT/mu_spec_mean_en2.pdf")
plot(ntrad_x, ntrad_y, xlim=c(0.14,0.2), ylim=c( 0.05,0.10), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, col=group, cex=.7)
dev.off()
pdf("PLOT/mu_spec_median_en1.pdf")
plot(ntrad_x, ntrad_y, xlim=c(0.0,0.5), ylim=c(-0.25,0.25), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, col=itemlabel, cex=.7)
dev.off()
pdf("PLOT/mu_spec_mean_en2.pdf")
plot(ntrad_x, ntrad_y, xlim=c(0.14,0.2), ylim=c( 0.05,0.10), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, col=group, cex=.7)
dev.off()

#---------------------------------------------------------------------------------------------------------------
# Spectral Clustering for Combined Estimated Item Distance Matrices After Removing 5 Items
#---------------------------------------------------------------------------------------------------------------
# 1. Remove 5 Items
exempt = c(1, 6, 33, 57, 72)
exempt = sort(exempt)
inside = c(1:72)
inside = inside[-exempt]
ndist = idist[-exempt,-exempt]

# 2. Spectral Clustering with K-means
Wnew = affinityMatrix(ndist,K=(length(inside)-1)/2)
ncluster_new = 3
if(option==1){
  group_new = spectralClustering(Wnew,ncluster_new);
  write.table(group_new,"RSUM/group_new.txt",row.names=FALSE,col.names=FALSE)
}else{
  group_new = scan("RSUM/group_new.txt")
}
table(group_new)

# 3. Spectral Clustering with K-median
d = rowSums(Wnew)
d[d == 0] = .Machine$double.eps
D = diag(d)
L = D - Wnew
Di = diag(1 / sqrt(d))
NL = Di %*% L %*% Di
eig = eigen(NL)
res = sort(abs(eig$values),index.return = TRUE)
U = eig$vectors[,res$ix[1:ncluster_new]]
normalize <- function(x) x / sqrt(sum(x^2))
U = t(apply(U,1,normalize))
item_newlabel = kcca(U, ncluster_new, family=kccaFamily("kmedians"))
if(option==1){
  itemnewlabel = item_newlabel@cluster
  write.table(itemnewlabel,"RSUM/group_med_new.txt",row.names=FALSE,col.names=FALSE)
}else{
  itemnewlabel = scan("RSUM/group_med_new.txt")
}

# 4. Plot Spectral Clustering (with K-mean) Results
pdf("PLOT/exmu_spec_mean.pdf")
plot(ntrad_x[inside], ntrad_y[inside], xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x[inside], ntrad_y[inside], labels=inside, col=group_new, cex=.7)
dev.off()

pdf("PLOT/exmu_spec_mean.pdf")
plot(ntrad_x[inside], ntrad_y[inside], xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x[inside], ntrad_y[inside], labels=inside, col=itemnewlabel, cex=.7)
dev.off()

pdf("PLOT/exmu_spec_mean_en.pdf")
plot(ntrad_x[inside], ntrad_y[inside], xlim=c(0.0,0.5), ylim=c(-0.25,0.25), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x[inside], ntrad_y[inside], labels=inside, col=group_new, cex=.7)
dev.off()

pdf("PLOT/exmu_spec_median_en.pdf")
plot(ntrad_x[inside], ntrad_y[inside], xlim=c(0.0,0.5), ylim=c(-0.25,0.25), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_x[inside], ntrad_y[inside], labels=inside, col=group_new, cex=.7)
dev.off()

#---------------------------------------------------------------------------------------------------------------
# Construct School Distance Matrix Using Estimated Item Distance Matrices
# Construct School Distance Matrix Using Post-Processed Item Latent Spaces
#---------------------------------------------------------------------------------------------------------------
sdist = matrix(scan("RESULT/sum_a1.log"),ncol=nschool,byrow=TRUE)
if(option == 1){
  spoint = isoMDS(sdist,k=2)
  spoint_x = spoint$points[,1]; spoint_y = spoint$points[,2];
  write.table(spoint$points,"RSUM/school_pos.txt",row.names=FALSE,col.names=FALSE)
}else{
  temp = matrix(scan("RSUM/school_pos.txt"),ncol=2,byrow=TRUE)
  spoint_x = temp[,1]; spoint_y = temp[,2];
}

pdf("PLOT/school.pdf")
plot(spoint_x, spoint_y, xlim=c(-12.5,10.0), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(spoint_x, spoint_y, labels=1:nschool, cex=.7)
dev.off()

#---------------------------------------------------------------------------------------------------------------
# Spectral Clustering for School Distance Matrix Using Estimated Item Distance Matrices
#---------------------------------------------------------------------------------------------------------------
# 1. Spectral Clustering (k-mean) for School Dependence Matrices
school_ndist = affinityMatrix(sdist,K=(nschool-2)/2)
ncluster_new = 2
if(option==1){
  sgroup = spectralClustering(school_ndist,ncluster_new);
  write.table(sgroup,"RSUM/school_group.txt",row.names=FALSE,col.names=FALSE)
}else{
  sgroup = scan("RSUM/school_group.txt")
}
table(sgroup)

sdist1 = sdist[-10,]
sdist1 = sdist1[,-10]

slabel = 1:nschool
slabel = slabel[-10]

spoint_xnew = spoint_x[-10]
spoint_ynew = spoint_y[-10]
spoint_xnew_avg = mean(spoint_xnew)
spoint_ynew_avg = mean(spoint_ynew)
spoint_xnew_sd = sd(spoint_xnew)
spoint_ynew_sd = sd(spoint_ynew)

spoint_xnew_standard = (spoint_xnew - spoint_xnew_avg) / spoint_xnew_sd
spoint_ynew_standard = (spoint_ynew - spoint_ynew_avg) / spoint_ynew_sd

plot(spoint_xnew_standard, spoint_ynew_standard, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(spoint_xnew_standard, spoint_ynew_standard, labels=slabel[-10], col=temp*1+1, cex=.7)

temp = abs(spoint_xnew_standard) < 0.5 & abs(spoint_ynew_standard) < 0.5

# 2. Plot Spectral Clustering (with K-mean) Results
pdf("PLOT/school_group_mean.pdf")
plot(spoint_x, spoint_y, xlim=c(-12.5,10.0), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(spoint_x, spoint_y, labels=1:nschool, col=sgroup, cex=.7)
dev.off()

# 3. Plot Renovation School
renov = scan("DATA/renov.txt")
pdf("PLOT/school_renov.pdf")
plot(spoint_x, spoint_y, xlim=c(-12.5,10.0), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(spoint_x, spoint_y, labels=1:nschool, col=renov+1, cex=.7)
dev.off()

# 4. Spectral Clustering (k-median) for School Dependence Matrices
d = rowSums(school_ndist)
d[d == 0] = .Machine$double.eps
D = diag(d)
L = D - school_ndist
Di = diag(1 / sqrt(d))
NL = Di %*% L %*% Di
eig = eigen(NL)
res = sort(abs(eig$values),index.return = TRUE)
U = eig$vectors[,res$ix[1:ncluster_new]]
normalize <- function(x) x / sqrt(sum(x^2))
U = t(apply(U,1,normalize))
school_label = kcca(U, ncluster_new, family=kccaFamily("kmedians"))
if(option==1){
  sgroup_med = school_label@cluster
  write.table(sgroup_med,"RSUM/school_med_group.txt",row.names=FALSE,col.names=FALSE)
}else{
  sgroup_med = scan("RSUM/school_med_group.txt")
}

# 5. Plot Spectral Clustering (with K-mean) Results
pdf("PLOT/school_group_median.pdf")
plot(spoint_x, spoint_y, xlim=c(-12.5,10.0), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(spoint_x, spoint_y, labels=1:nschool, col=sgroup_med, cex=.7)
dev.off()

index = 0;
resp = matrix(0,sum(count),2)
schl = rep(NA,sum(count))
for(a in 1:nschool){
  if(a < 10){fname = paste("DATA/item0",a,".txt",sep="")}
  else{fname = paste("DATA/item",a,".txt",sep="")}
  temp = matrix(scan(fname),ncol=nitem,byrow=TRUE)
  for(i in 1:count[a]){
    resp[index+i,1] = temp[i,]%*%ntrad_x /sum(temp[i,])
    resp[index+i,2] = temp[i,]%*%ntrad_y /sum(temp[i,])
    schl[index+i] = a
  }
  index = index+count[a]
}

pdf("PLOT/reconstruct1.pdf")
plot(resp[1:count[1],1],resp[1:count[1],2], xlim=c(-0.5,0.5), ylim=c(-1,1),
     xlab="Coordinate 1", ylab="Coordinate 2", type="n")
index = 0
for(i in 1:nschool){
  if(renov[i]==0) points(resp[(index+1):(index+count[i]),1], resp[(index+1):(index+count[i]),2], pch=20, col=renov[i]+1, cex=0.5)
  if(renov[i]==1) points(resp[(index+1):(index+count[i]),1], resp[(index+1):(index+count[i]),2], pch=20, col=renov[i]+1, cex=0.5)
  index = index+count[i]
} 
dev.off()

pdf("PLOT/reconstruct2.pdf")
plot(resp[1:count[1],1],resp[1:count[1],2], xlim=c(-0.5,0.5), ylim=c(-1,1),
     xlab="Coordinate 1", ylab="Coordinate 2", type="n")
index = 0
for(i in 1:nschool){
  if(renov[i]==0) points(resp[(index+1):(index+count[i]),1], resp[(index+1):(index+count[i]),2], pch=20, col=renov[i]+1, cex=0.5)
  if(renov[i]==1) points(resp[(index+1):(index+count[i]),1], resp[(index+1):(index+count[i]),2], pch=20, col=renov[i]+1, cex=0.5)
  index = index+count[i]
} 
dev.off()

index = 0; school_avg = matrix(NA,nschool,2);
for(i in 1:nschool){
  school_avg[i,1] = mean(resp[(index+1):(index+count[i]),1])
  school_avg[i,2] = mean(resp[(index+1):(index+count[i]),2])
  index = index + count[i]
}
pdf("PLOT/school_recon1.pdf")
plot(school_avg[,1], school_avg[,2], xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(school_avg[,1], school_avg[,2], labels=1:nschool, col=renov+1, cex=.7)
dev.off()

index = 0; school_avg = matrix(NA,nschool,2);
for(i in 1:nschool){
  school_avg[i,1] = median(resp[(index+1):(index+count[i]),1])
  school_avg[i,2] = median(resp[(index+1):(index+count[i]),2])
  index = index + count[i]
}
pdf("PLOT/school_recon2.pdf")
plot(school_avg[,1], school_avg[,2], xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(school_avg[,1], school_avg[,2], labels=1:nschool, col=renov+1, cex=.7)
dev.off()

pdf("PLOT/resp_recon_item.pdf")
plot(resp[1:count[1],1],resp[1:count[1],2], xlim=c(-5,2), ylim=c(-5, 5),
     xlab="Coordinate 1", ylab="Coordinate 2", type="n")
index = 0
for(i in 1:nschool){
  if(renov[i]==0) points(resp[(index+1):(index+count[i]),1], resp[(index+1):(index+count[i]),2], pch=".", col=renov[i]+1)
  if(renov[i]==1) points(resp[(index+1):(index+count[i]),1], resp[(index+1):(index+count[i]),2], pch=".", col=renov[i]+1)
  index = index+count[i]
} 
text(ntrad_x, ntrad_y, labels=1:nitem, cex=.7, col=4)
dev.off()

school_dist = as.matrix(dist(school_avg))
Wschool = affinityMatrix(school_dist,K=(nschool-2)/2)
ncluster_school = 2
if(option==1){
  group_school = spectralClustering(Wschool,ncluster_school);
  write.table(group_school,"RSUM/group_school.txt",row.names=FALSE,col.names=FALSE)
}else{
  group_school = scan("RSUM/group_school.txt")
}
table(group_school)

pdf("PLOT/school_spec.pdf")
plot(school_avg[,1], school_avg[,2], xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(school_avg[,1], school_avg[,2], labels=1:nschool, col=group_school, cex=.7)
dev.off()

ntrad_xnew = (ntrad_x - mean(ntrad_x)) / sd(ntrad_x)
ntrad_ynew = (ntrad_y - mean(ntrad_y)) / sd(ntrad_y)
school_xnew = (school_avg[,1] - mean(school_avg[,1])) / sd(school_avg[,1])
school_ynew = (school_avg[,2] - mean(school_avg[,2])) / sd(school_avg[,2])

pdf("PLOT/item_school.pdf")
plot(ntrad_xnew, ntrad_ynew, xlim = c(-5,2.5), xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(ntrad_xnew, ntrad_ynew, labels=1:nschool, col=2, cex=.7)
text(school_xnew, school_ynew, labels=1:nschool, col=4, cex=.7)
dev.off()

# plot(ntrad_x, ntrad_y, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
# text(ntrad_x, ntrad_y, labels=1:nitem, col=1, cex=.7)
# text(((school_avg[,1]-mean(school_avg[,1]))+0.02)*10, (school_avg[,2]-mean(school_avg[,2]))*10, labels=1:nschool, col=2, cex=.7)

school_new = data.frame(y=temp, poor=scov[-10,2], meal=scov[-10,3],
                    govern=scov[-10,4],class=scov[-10,5],coed=scov[-10,6],type=scov[-10,7],college=scov[-10,8])
school_logis = glm(y~poor+meal+govern+class+coed+type+college,family=binomial(link=logit),data=school_new)
summary(school_logis)

library(ggplot2)
library(ggrepel)
scov = matrix(scan("DATA/scov.txt"),ncol=8,byrow=TRUE)
school = data.frame(x=school_avg[,1], y=school_avg[,2], poor=scov[,2], meal=scov[,3],
                    govern=scov[,4],class=scov[,5],coed=scov[,6],type=scov[,7],college=scov[,8])
renov_col = renov
renov_col[renov==1] = 2
renov_col[renov==0] = 4
p = ggplot(school, aes(x,y))
pdf("PLOT/rpoor.pdf"); p + geom_point(aes(size=poor),colour=renov_col,shape=1); dev.off()
pdf("PLOT/rmeal.pdf"); p + geom_point(aes(size=meal),colour=renov_col,shape=1); dev.off()
pdf("PLOT/rgovn.pdf"); p + geom_point(aes(size=govern),colour=renov_col,shape=1); dev.off()
pdf("PLOT/rclass.pdf"); p + geom_point(aes(size=class),colour=renov_col,shape=1); dev.off()
pdf("PLOT/rcol.pdf"); p + geom_point(aes(size=college),colour=renov_col,shape=1); dev.off()
pdf("PLOT/rcoed.pdf"); p + geom_point(aes(size=poor),colour=scov[,6]+1,shape=20); dev.off()
pdf("PLOT/rtype.pdf"); p + geom_point(aes(size=poor),colour=scov[,7]+1,shape=20); dev.off()

school = data.frame(x=spoint_x, y=spoint_y, poor=scov[,2], meal=scov[,3],
                    govern=scov[,4],class=scov[,5],coed=scov[,6],type=scov[,7],college=scov[,8])
p = ggplot(school, aes(x,y))
pdf("PLOT/ipoor.pdf"); p + geom_point(aes(size=poor),colour=renov_col,shape=1) + xlim(-15,15.0); dev.off()
pdf("PLOT/imeal.pdf"); p + geom_point(aes(size=meal),colour=renov_col,shape=1) + xlim(-15,15.0); dev.off()
pdf("PLOT/igovn.pdf"); p + geom_point(aes(size=govern),colour=renov_col,shape=1) + xlim(-15,15.0); dev.off()
pdf("PLOT/iclass.pdf"); p + geom_point(aes(size=class),colour=renov_col,shape=1) + xlim(-15,15.0); dev.off()
pdf("PLOT/icol.pdf"); p + geom_point(aes(size=college),colour=renov_col,shape=1) + xlim(-15,15.0); dev.off()
pdf("PLOT/icoed.pdf"); p + geom_point(aes(size=poor),colour=scov[,6]+1,shape=20) + xlim(-15,15.0); dev.off()
pdf("PLOT/itype.pdf"); p + geom_point(aes(size=poor),colour=scov[,7]+1,shape=20) + xlim(-15,15.0); dev.off()


---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
# Draw HeatMap Using Each School's Estimated Item Distance Matrices (After Removing Random Effects)
# Recover Positions from Each School's Estimated Item Distance Matrices Using Multidimensional Scaling
# Draw HeatMap Using Post-Processed Item Latent Spaces
#---------------------------------------------------------------------------------------------------------------
temp = matrix(scan("RESULT/sum_m1.log"),ncol=ndyad,byrow=TRUE)
for(k in 1:nschool){
  tmp.school = temp[k,1:ndyad];
  idist.school = matrix(0,nitem,nitem);
  index = 0;
  for(i in 2:nitem){
    for(j in 1:(i-1)){
      index = index + 1
      idist.school[i,j] = exp(temp_school[index])
    }
  }
  idist.school = idist.school + t(idist.school)

  # if(k < 10){fname = paste("RSUM/point0",k,".pdf",sep="")}
  # else{fname = paste("RUM/point",k,".pdf",sep="")}
  # pdf(fname);
  # heatmap(idist_school, symm=TRUE, keep.dendro=FALSE, cexRow=0.5, cexCol=0.5);
  # dev.off();

  # if(k < 10){fname = paste("PLOT/wheatmap0",k,".pdf",sep="")}
  # else{fname = paste("PLOT/wheatmap",k,".pdf",sep="")}
  # pdf(fname);
  # heatmap(Wfinal_dist[k,,], symm=TRUE, keep.dendro=FALSE, cexRow=0.5, cexCol=0.5);
  # dev.off();

  if(k < 10){fname = paste("RSUM/position0",k,".txt",sep="")}
  else{fname = paste("RSUM/position",k,".txt",sep="")}
  ntrad_school = isoMDS(idist.school,k=2)
  ntrad_school_x = ntrad_school$points[,1]; ntrad_school_y = ntrad_school$points[,2];
  write.table(ntrad_school$points,fname,row.names=FALSE,col.names=FALSE)

  if(k < 10){fname = paste("PLOT/mu_xy0",k,".pdf",sep="")}
  else{fname = paste("PLOT/mu_xy",k,".pdf",sep="")}
  pdf(fname)
  plot(ntrad_school_x, ntrad_school_y, xlab="Coordinate 1", ylab="Coordinate 2", main="Nonparametric MDS", type="n")
  text(ntrad_school_x, ntrad_school_y, labels=1:nitem, cex=.7)
  dev.off()
}

#---------------------------------------------------------------------------------------------------------------
# Save Computation Result
#---------------------------------------------------------------------------------------------------------------
save.image("GEPS-MODEL1-RUN1.RData")
