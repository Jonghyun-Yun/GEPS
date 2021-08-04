#---------------------------------------------------------------------------------------------------------------
# Basic Settings // Data Loading
#---------------------------------------------------------------------------------------------------------------
#setwd("~/Documents/BACKUP/MODEL1/RUN1")
setwd("~/Dropbox/Collaborate/MJEON/ANALYSIS/GEPS/MODEL1/RUN1")
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
plot(ntrad_x, ntrad_y, xlab="", ylab="", type="n")
text(ntrad_x, ntrad_y, labels=1:nitem, cex=1.5)
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
plot(spoint_x, spoint_y, type="n")
text(spoint_x, spoint_y, labels=1:nschool, cex=1.5)
dev.off()
# xlim=c(-12.5,10.0), 

# 3. Plot Renovation School
renov = scan("DATA/renov.txt")
pdf("PLOT/school_renov.pdf")
plot(spoint_x, spoint_y, xlab="", ylab="", type="n")
text(spoint_x, spoint_y, labels=1:nschool, col=renov+1, cex=1.5)
dev.off()
# xlim=c(-12.5,10.0), 

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
plot(school_avg[,1], school_avg[,2], xlab="", ylab="", type="n")
text(school_avg[,1], school_avg[,2], labels=1:nschool, col=renov+1, cex=1.5)
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
plot(school_avg[,1], school_avg[,2], xlab="", ylab="", type="n")
text(school_avg[,1], school_avg[,2], labels=1:nschool, col=group_school, cex=1.5)
dev.off()

ntrad_xnew = (ntrad_x - mean(ntrad_x)) / sd(ntrad_x)
ntrad_ynew = (ntrad_y - mean(ntrad_y)) / sd(ntrad_y)
school_xnew = (school_avg[,1] - mean(school_avg[,1])) / sd(school_avg[,1])
school_ynew = (school_avg[,2] - mean(school_avg[,2])) / sd(school_avg[,2])

pdf("PLOT/item_school.pdf")
plot(ntrad_xnew, ntrad_ynew, xlim = c(-5,2.5), xlab="", ylab="", type="n")
text(ntrad_xnew, ntrad_ynew, labels=1:nschool, col=2, cex=1.5)
text(school_xnew, school_ynew, labels=1:nschool, col=4, cex=1.5)
dev.off()

# plot(ntrad_x, ntrad_y, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
# text(ntrad_x, ntrad_y, labels=1:nitem, col=1, cex=.7)
# text(((school_avg[,1]-mean(school_avg[,1]))+0.02)*10, (school_avg[,2]-mean(school_avg[,2]))*10, labels=1:nschool, col=2, cex=.7)

school_new = data.frame(y=temp, poor=scov[-10,2], meal=scov[-10,3],
                    govern=scov[-10,4],class=scov[-10,5],coed=scov[-10,6],type=scov[-10,7],college=scov[-10,8])
school_logis = glm(y~poor+meal+govern+class+coed+type+college,family=binomial(link=logit),data=school_new)
summary(school_logis)

# save.image("GEPS-MODEL1-RUN1.RData")

item_ls = array(0, dim=c(nschool, nitem, nitem))
item_recover = array(0, dim=c(nschool, nitem, nitem))
for(k in 1:nschool){
  if(k < 10) fname1 = paste("RESULT/sum0",k,"_d1.log",sep="")
  else fname1 = paste("RESULT/sum",k,"_d1.log",sep="")
  temp = as.matrix(read.table(fname1))
  index = 0
  for(i in 2:nitem){
    for(j in 1:(i-1)){
      index = index + 1
      if(temp[1,index] > 0) {item_ls[k,i,j] = temp[1,index]} else {item_ls[k,i,j] = 0.000001}
    }
  }
  item_ls[k,,] = item_ls[k,,] + t(item_ls[k,,])
}

library(MASS)
pdf("PLOT/item_ls.pdf")
for(k in 1:nschool){
  temp = as.dist(item_ls[k,,])
  fit = cmdscale(temp, eig=TRUE, k=2)
  item_recover[k,,] = fit$points
  plot(item_recover[k,,], xlab="n", ylab="n", type="n")
  text(item_recover[k,,], labels = 1:nitem)
  print(k)
}
dev.off()

library("Rcpp")
library(ISLR)
library(pROC)
library(ROCR)
sourceCpp(file="pp.cpp")
# 
# # --------------------------------------------------------------------------------------
# # rbinom
# # --------------------------------------------------------------------------------------
# 
# auc.Y = matrix(NA,nschool,niter); sen.Y = matrix(NA,nschool,niter)
# spe.Y = matrix(NA,nschool,niter); ove.Y = matrix(NA,nschool,niter)
# auc.U = matrix(NA,nschool,niter); sen.U = matrix(NA,nschool,niter)
# spe.U = matrix(NA,nschool,niter); ove.U = matrix(NA,nschool,niter)
# phi.Y = matrix(NA,nschool,niter); phi.U = matrix(NA,nschool,niter)
# 
# for(a in 1:nschool){
#   beta_temp = sim_beta[a,,]
#   theta_temp = sim_theta[a,,]
#   z_temp = array(0, dim=c(niter,count[a],ndim))
#   w_temp = array(0, dim=c(niter,nitem,ndim))
#   for(iter in 1:niter){
#     z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
#   }
#   gen = pp(Y[a,,,], U[a,,,], beta_temp, theta_temp, z_temp, w_temp, niter, count[a], nitem, ndim, 1, 0.5)
# 
#   for(i in 1:niter){
#     temp.table = table(predicted = gen$Y_pred[i,], actual = gen$Y_orig)
#     TN.Y = temp.table[1,1] / sum(temp.table)
#     TP.Y = temp.table[2,2] / sum(temp.table)
#     FP.Y = temp.table[2,1] / sum(temp.table)
#     FN.Y = temp.table[1,2] / sum(temp.table)
#     spe.Y[a,i] = TN.Y /(TN.Y+FP.Y)
#     sen.Y[a,i] = TP.Y /(FN.Y+TP.Y)
#     ove.Y[a,i] = (TP.Y + TN.Y)
#     phi.Y[a,i] = (TP.Y * TN.Y + FP.Y * FN.Y) / sqrt((TP.Y + FN.Y) * (TN.Y + FP.Y) * (TP.Y + FP.Y) * (TN.Y + FN.Y))
#     temp = roc(gen$Y_orig, gen$Y_pred[i,])
#     auc.Y[a,i] = auc(temp)
# 
#     temp.table = table(predicted = gen$U_pred[i,], actual = gen$U_orig)
#     TN.U = temp.table[1,1] / sum(temp.table)
#     TP.U = temp.table[2,2] / sum(temp.table)
#     FP.U = temp.table[2,1] / sum(temp.table)
#     FN.U = temp.table[1,2] / sum(temp.table)
#     spe.U[a,i] = TN.U /(TN.U+FP.U)
#     sen.U[a,i] = TP.U /(FN.U+TP.U)
#     ove.U[a,i] = (TP.U + TN.U)
#     phi.U[a,i] = (TP.U * TN.U + FP.U * FN.U) / sqrt((TP.U + FN.U) * (TN.U + FP.U) * (TP.U + FP.U) * (TN.U + FN.U))
#     temp = roc(gen$U_orig, gen$U_pred[i,])
#     auc.U[a,i] = auc(temp)
#     print(c(a,i,spe.Y[a,i],spe.U[a,i],sen.Y[a,i],sen.U[a,i],auc.Y[a,i],auc.U[a,i]))
#   }
# }
# write.table(round(spe.Y, 3), "RSUM/spe_Y.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.Y, 3), "RSUM/sen_Y.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.Y, 3), "RSUM/ove_Y.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.Y, 3), "RSUM/phi_Y.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.Y, 3), "RSUM/auc_Y.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(spe.U, 3), "RSUM/spe_U.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.U, 3), "RSUM/sen_U.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.U, 3), "RSUM/ove_U.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.U, 3), "RSUM/phi_U.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.U, 3), "RSUM/auc_U.txt", row.names=FALSE, col.names=FALSE)
# 
# # --------------------------------------------------------------------------------------
# # 0.10
# # --------------------------------------------------------------------------------------
# 
# auc.Y.10 = matrix(NA,nschool,niter); sen.Y.10 = matrix(NA,nschool,niter)
# spe.Y.10 = matrix(NA,nschool,niter); ove.Y.10 = matrix(NA,nschool,niter)
# auc.U.10 = matrix(NA,nschool,niter); sen.U.10 = matrix(NA,nschool,niter)
# spe.U.10 = matrix(NA,nschool,niter); ove.U.10 = matrix(NA,nschool,niter)
# phi.Y.10 = matrix(NA,nschool,niter); phi.U.10 = matrix(NA,nschool,niter)
# 
# for(a in 1:nschool){
#   beta_temp = sim_beta[a,,]
#   theta_temp = sim_theta[a,,]
#   z_temp = array(0, dim=c(niter,count[a],ndim))
#   w_temp = array(0, dim=c(niter,nitem,ndim))
#   for(iter in 1:niter){
#     z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
#   }
#   gen = pp(Y[a,,,], U[a,,,], beta_temp, theta_temp, z_temp, w_temp, niter, count[a], nitem, ndim, 0, 0.1)
#   
#   for(i in 1:niter){
#     temp.table = table(predicted = gen$Y_pred[i,], actual = gen$Y_orig)
#     TN.Y = temp.table[1,1] / sum(temp.table)
#     TP.Y = temp.table[2,2] / sum(temp.table)
#     FP.Y = temp.table[2,1] / sum(temp.table)
#     FN.Y = temp.table[1,2] / sum(temp.table)
#     spe.Y.10[a,i] = TN.Y /(TN.Y+FP.Y)
#     sen.Y.10[a,i] = TP.Y /(FN.Y+TP.Y)
#     ove.Y.10[a,i] = (TP.Y + TN.Y)
#     phi.Y.10[a,i] = (TP.Y * TN.Y + FP.Y * FN.Y) / sqrt((TP.Y + FN.Y) * (TN.Y + FP.Y) * (TP.Y + FP.Y) * (TN.Y + FN.Y))
#     temp = roc(gen$Y_orig, gen$Y_pred[i,])
#     auc.Y.10[a,i] = auc(temp)
#     
#     temp.table = table(predicted = gen$U_pred[i,], actual = gen$U_orig)
#     TN.U = temp.table[1,1] / sum(temp.table)
#     TP.U = temp.table[2,2] / sum(temp.table)
#     FP.U = temp.table[2,1] / sum(temp.table)
#     FN.U = temp.table[1,2] / sum(temp.table)
#     spe.U.10[a,i] = TN.U /(TN.U+FP.U)
#     sen.U.10[a,i] = TP.U /(FN.U+TP.U)
#     ove.U.10[a,i] = (TP.U + TN.U)
#     phi.U.10[a,i] = (TP.U * TN.U + FP.U * FN.U) / sqrt((TP.U + FN.U) * (TN.U + FP.U) * (TP.U + FP.U) * (TN.U + FN.U))
#     temp = roc(gen$U_orig, gen$U_pred[i,])
#     auc.U.10[a,i] = auc(temp)
#     print(c(a,i,spe.Y.10[a,i],spe.U.10[a,i],sen.Y.10[a,i],sen.U.10[a,i],auc.Y.10[a,i],auc.U.10[a,i]))
#   }
# }
# write.table(round(spe.Y.10, 3), "RSUM/spe_Y_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.Y.10, 3), "RSUM/sen_Y_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.Y.10, 3), "RSUM/ove_Y_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.Y.10, 3), "RSUM/phi_Y_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.Y.10, 3), "RSUM/auc_Y_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(spe.U.10, 3), "RSUM/spe_U_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.U.10, 3), "RSUM/sen_U_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.U.10, 3), "RSUM/ove_U_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.U.10, 3), "RSUM/phi_U_10.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.U.10, 3), "RSUM/auc_U_10.txt", row.names=FALSE, col.names=FALSE)
# 
# # --------------------------------------------------------------------------------------
# # 0.25
# # --------------------------------------------------------------------------------------
# 
# auc.Y.25 = matrix(NA,nschool,niter); sen.Y.25 = matrix(NA,nschool,niter)
# spe.Y.25 = matrix(NA,nschool,niter); ove.Y.25 = matrix(NA,nschool,niter)
# auc.U.25 = matrix(NA,nschool,niter); sen.U.25 = matrix(NA,nschool,niter)
# spe.U.25 = matrix(NA,nschool,niter); ove.U.25 = matrix(NA,nschool,niter)
# phi.Y.25 = matrix(NA,nschool,niter); phi.U.25 = matrix(NA,nschool,niter)
# 
# for(a in 1:nschool){
#   beta_temp = sim_beta[a,,]
#   theta_temp = sim_theta[a,,]
#   z_temp = array(0, dim=c(niter,count[a],ndim))
#   w_temp = array(0, dim=c(niter,nitem,ndim))
#   for(iter in 1:niter){
#     z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
#   }
#   gen = pp(Y[a,,,], U[a,,,], beta_temp, theta_temp, z_temp, w_temp, niter, count[a], nitem, ndim, 0, 0.25)
#   
#   for(i in 1:niter){
#     temp.table = table(predicted = gen$Y_pred[i,], actual = gen$Y_orig)
#     TN.Y = temp.table[1,1] / sum(temp.table)
#     TP.Y = temp.table[2,2] / sum(temp.table)
#     FP.Y = temp.table[2,1] / sum(temp.table)
#     FN.Y = temp.table[1,2] / sum(temp.table)
#     spe.Y.25[a,i] = TN.Y /(TN.Y+FP.Y)
#     sen.Y.25[a,i] = TP.Y /(FN.Y+TP.Y)
#     ove.Y.25[a,i] = (TP.Y + TN.Y)
#     phi.Y.25[a,i] = (TP.Y * TN.Y + FP.Y * FN.Y) / sqrt((TP.Y + FN.Y) * (TN.Y + FP.Y) * (TP.Y + FP.Y) * (TN.Y + FN.Y))
#     temp = roc(gen$Y_orig, gen$Y_pred[i,])
#     auc.Y.25[a,i] = auc(temp)
#     
#     temp.table = table(predicted = gen$U_pred[i,], actual = gen$U_orig)
#     TN.U = temp.table[1,1] / sum(temp.table)
#     TP.U = temp.table[2,2] / sum(temp.table)
#     FP.U = temp.table[2,1] / sum(temp.table)
#     FN.U = temp.table[1,2] / sum(temp.table)
#     spe.U.25[a,i] = TN.U /(TN.U+FP.U)
#     sen.U.25[a,i] = TP.U /(FN.U+TP.U)
#     ove.U.25[a,i] = (TP.U + TN.U)
#     phi.U.25[a,i] = (TP.U * TN.U + FP.U * FN.U) / sqrt((TP.U + FN.U) * (TN.U + FP.U) * (TP.U + FP.U) * (TN.U + FN.U))
#     temp = roc(gen$U_orig, gen$U_pred[i,])
#     auc.U.25[a,i] = auc(temp)
#     print(c(a,i,spe.Y.25[a,i],spe.U.25[a,i],sen.Y.25[a,i],sen.U.25[a,i],auc.Y.25[a,i],auc.U.25[a,i]))
#   }
# }
# write.table(round(spe.Y.25, 3), "RSUM/spe_Y_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.Y.25, 3), "RSUM/sen_Y_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.Y.25, 3), "RSUM/ove_Y_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.Y.25, 3), "RSUM/phi_Y_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.Y.25, 3), "RSUM/auc_Y_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(spe.U.25, 3), "RSUM/spe_U_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.U.25, 3), "RSUM/sen_U_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.U.25, 3), "RSUM/ove_U_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.U.25, 3), "RSUM/phi_U_25.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.U.25, 3), "RSUM/auc_U_25.txt", row.names=FALSE, col.names=FALSE)
# 
# # --------------------------------------------------------------------------------------
# # 0.30
# # --------------------------------------------------------------------------------------
# 
# auc.Y.30 = matrix(NA,nschool,niter); sen.Y.30 = matrix(NA,nschool,niter)
# spe.Y.30 = matrix(NA,nschool,niter); ove.Y.30 = matrix(NA,nschool,niter)
# auc.U.30 = matrix(NA,nschool,niter); sen.U.30 = matrix(NA,nschool,niter)
# spe.U.30 = matrix(NA,nschool,niter); ove.U.30 = matrix(NA,nschool,niter)
# phi.Y.30 = matrix(NA,nschool,niter); phi.U.30 = matrix(NA,nschool,niter)
# 
# for(a in 1:nschool){
#   beta_temp = sim_beta[a,,]
#   theta_temp = sim_theta[a,,]
#   z_temp = array(0, dim=c(niter,count[a],ndim))
#   w_temp = array(0, dim=c(niter,nitem,ndim))
#   for(iter in 1:niter){
#     z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
#   }
#   gen = pp(Y[a,,,], U[a,,,], beta_temp, theta_temp, z_temp, w_temp, niter, count[a], nitem, ndim, 0, 0.3)
#   
#   for(i in 1:niter){
#     temp.table = table(predicted = gen$Y_pred[i,], actual = gen$Y_orig)
#     TN.Y = temp.table[1,1] / sum(temp.table)
#     TP.Y = temp.table[2,2] / sum(temp.table)
#     FP.Y = temp.table[2,1] / sum(temp.table)
#     FN.Y = temp.table[1,2] / sum(temp.table)
#     spe.Y.30[a,i] = TN.Y /(TN.Y+FP.Y)
#     sen.Y.30[a,i] = TP.Y /(FN.Y+TP.Y)
#     ove.Y.30[a,i] = (TP.Y + TN.Y)
#     phi.Y.30[a,i] = (TP.Y * TN.Y + FP.Y * FN.Y) / sqrt((TP.Y + FN.Y) * (TN.Y + FP.Y) * (TP.Y + FP.Y) * (TN.Y + FN.Y))
#     temp = roc(gen$Y_orig, gen$Y_pred[i,])
#     auc.Y.30[a,i] = auc(temp)
#     
#     temp.table = table(predicted = gen$U_pred[i,], actual = gen$U_orig)
#     TN.U = temp.table[1,1] / sum(temp.table)
#     TP.U = temp.table[2,2] / sum(temp.table)
#     FP.U = temp.table[2,1] / sum(temp.table)
#     FN.U = temp.table[1,2] / sum(temp.table)
#     spe.U.30[a,i] = TN.U /(TN.U+FP.U)
#     sen.U.30[a,i] = TP.U /(FN.U+TP.U)
#     ove.U.30[a,i] = (TP.U + TN.U)
#     phi.U.30[a,i] = (TP.U * TN.U + FP.U * FN.U) / sqrt((TP.U + FN.U) * (TN.U + FP.U) * (TP.U + FP.U) * (TN.U + FN.U))
#     temp = roc(gen$U_orig, gen$U_pred[i,])
#     auc.U.30[a,i] = auc(temp)
#     print(c(a,i,spe.Y.30[a,i],spe.U.30[a,i],sen.Y.30[a,i],sen.U.30[a,i],auc.Y.30[a,i],auc.U.30[a,i]))
#   }
# }
# write.table(round(spe.Y.30, 3), "RSUM/spe_Y_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.Y.30, 3), "RSUM/sen_Y_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.Y.30, 3), "RSUM/ove_Y_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.Y.30, 3), "RSUM/phi_Y_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.Y.30, 3), "RSUM/auc_Y_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(spe.U.30, 3), "RSUM/spe_U_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.U.30, 3), "RSUM/sen_U_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.U.30, 3), "RSUM/ove_U_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.U.30, 3), "RSUM/phi_U_30.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.U.30, 3), "RSUM/auc_U_30.txt", row.names=FALSE, col.names=FALSE)
# 
# # --------------------------------------------------------------------------------------
# # 0.50
# # --------------------------------------------------------------------------------------
# 
# auc.Y.50 = matrix(NA,nschool,niter); sen.Y.50 = matrix(NA,nschool,niter)
# spe.Y.50 = matrix(NA,nschool,niter); ove.Y.50 = matrix(NA,nschool,niter)
# auc.U.50 = matrix(NA,nschool,niter); sen.U.50 = matrix(NA,nschool,niter)
# spe.U.50 = matrix(NA,nschool,niter); ove.U.50 = matrix(NA,nschool,niter)
# phi.Y.50 = matrix(NA,nschool,niter); phi.U.50 = matrix(NA,nschool,niter)
# 
# for(a in 1:nschool){
#   beta_temp = sim_beta[a,,]
#   theta_temp = sim_theta[a,,]
#   z_temp = array(0, dim=c(niter,count[a],ndim))
#   w_temp = array(0, dim=c(niter,nitem,ndim))
#   for(iter in 1:niter){
#     z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
#   }
#   gen = pp(Y[a,,,], U[a,,,], beta_temp, theta_temp, z_temp, w_temp, niter, count[a], nitem, ndim, 0, 0.5)
#   
#   for(i in 1:niter){
#     temp.table = table(predicted = gen$Y_pred[i,], actual = gen$Y_orig)
#     TN.Y = temp.table[1,1] / sum(temp.table)
#     TP.Y = temp.table[2,2] / sum(temp.table)
#     FP.Y = temp.table[2,1] / sum(temp.table)
#     FN.Y = temp.table[1,2] / sum(temp.table)
#     spe.Y.50[a,i] = TN.Y /(TN.Y+FP.Y)
#     sen.Y.50[a,i] = TP.Y /(FN.Y+TP.Y)
#     ove.Y.50[a,i] = (TP.Y + TN.Y)
#     phi.Y.50[a,i] = (TP.Y * TN.Y + FP.Y * FN.Y) / sqrt((TP.Y + FN.Y) * (TN.Y + FP.Y) * (TP.Y + FP.Y) * (TN.Y + FN.Y))
#     temp = roc(gen$Y_orig, gen$Y_pred[i,])
#     auc.Y.50[a,i] = auc(temp)
#     
#     temp.table = table(predicted = gen$U_pred[i,], actual = gen$U_orig)
#     TN.U = temp.table[1,1] / sum(temp.table)
#     TP.U = temp.table[2,2] / sum(temp.table)
#     FP.U = temp.table[2,1] / sum(temp.table)
#     FN.U = temp.table[1,2] / sum(temp.table)
#     spe.U.50[a,i] = TN.U /(TN.U+FP.U)
#     sen.U.50[a,i] = TP.U /(FN.U+TP.U)
#     ove.U.50[a,i] = (TP.U + TN.U)
#     phi.U.50[a,i] = (TP.U * TN.U + FP.U * FN.U) / sqrt((TP.U + FN.U) * (TN.U + FP.U) * (TP.U + FP.U) * (TN.U + FN.U))
#     temp = roc(gen$U_orig, gen$U_pred[i,])
#     auc.U.50[a,i] = auc(temp)
#     print(c(a,i,spe.Y.50[a,i],spe.U.50[a,i],sen.Y.50[a,i],sen.U.50[a,i],auc.Y.50[a,i],auc.U.50[a,i]))
#   }
# }
# write.table(round(spe.Y.50, 3), "RSUM/spe_Y_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.Y.50, 3), "RSUM/sen_Y_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.Y.50, 3), "RSUM/ove_Y_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.Y.50, 3), "RSUM/phi_Y_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.Y.50, 3), "RSUM/auc_Y_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(spe.U.50, 3), "RSUM/spe_U_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.U.50, 3), "RSUM/sen_U_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.U.50, 3), "RSUM/ove_U_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.U.50, 3), "RSUM/phi_U_50.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.U.50, 3), "RSUM/auc_U_50.txt", row.names=FALSE, col.names=FALSE)
# 
# 
# # --------------------------------------------------------------------------------------
# # Average
# # --------------------------------------------------------------------------------------
# w_temp = array(0, dim=c(nschool, nitem, nitem))
# for(k in 1:nschool){
#   if(k < 10) fname1 = paste("RESULT/sum0",k,"_d1.log",sep="")
#   else fname1 = paste("RESULT/sum",k,"_d1.log",sep="")
#   temp = as.matrix(read.table(fname1))
#   index = 0
#   for(i in 2:nitem){
#     for(j in 1:(i-1)){
#       index = index + 1
#       w_temp[k,i,j] = temp[1,index]
#     }
#   }
#   w_temp[k,,] = w_temp[k,,] + t(w_temp[k,,])
# }
# 
# nrep = 1000
# auc.Y.avg = matrix(NA,nschool,nrep); sen.Y.avg = matrix(NA,nschool,nrep) 
# spe.Y.avg = matrix(NA,nschool,nrep); ove.Y.avg = matrix(NA,nschool,nrep)
# auc.U.avg = matrix(NA,nschool,nrep); sen.U.avg = matrix(NA,nschool,nrep)
# spe.U.avg = matrix(NA,nschool,nrep); ove.U.avg = matrix(NA,nschool,nrep)
# phi.Y.avg = matrix(NA,nschool,nrep); phi.U.avg = matrix(NA,nschool,nrep)
# 
# for(a in 1:nschool){
#   beta_temp = apply(sim_beta[a,,], 2, mean)
#   theta_temp = apply(sim_theta[a,,1:count[a]], 2, mean)
#   z_temp = array(0, dim=c(niter,count[a],ndim))
#   z_dist_temp = array(0, dim=c(niter,count[a],count[a]))
#   z_dist = matrix(0,count[a],count[a])
#   for(iter in 1:niter){
#     z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
#     z_dist_temp[iter,,] = as.matrix(dist(z_temp[iter,,]))
#   }
#   for(i in 1:count[a]){
#     for(j in 1:count[a]){
#       z_dist[i,j] = mean(z_dist_temp[,i,j])
#     }
#   }
#   gen.avg = pp_mean(Y[a,,,], U[a,,,], beta_temp, theta_temp, z_dist, w_temp[a,,], nrep, count[a], nitem, ndim)
#   
#   for(i in 1:nrep){
#     temp.table = table(predicted = gen.avg$Y_pred[i,], actual = gen.avg$Y_orig)
#     TN.Y.avg = temp.table[1,1] / sum(temp.table)
#     TP.Y.avg = temp.table[2,2] / sum(temp.table)
#     FP.Y.avg = temp.table[2,1] / sum(temp.table)
#     FN.Y.avg = temp.table[1,2] / sum(temp.table)
#     spe.Y.avg[a,i] = TN.Y.avg /(TN.Y.avg+FP.Y.avg)
#     sen.Y.avg[a,i] = TP.Y.avg /(FN.Y.avg+TP.Y.avg)
#     ove.Y.avg[a,i] = (TP.Y.avg + TN.Y.avg)
#     phi.Y.avg[a,i] = (TP.Y.avg * TN.Y.avg + FP.Y.avg * FN.Y.avg) / sqrt((TP.Y.avg + FN.Y.avg) * (TN.Y.avg + FP.Y.avg) * (TP.Y.avg + FP.Y.avg) * (TN.Y.avg + FN.Y.avg))
#     temp = roc(gen.avg$Y_orig, gen.avg$Y_pred[i,])
#     auc.Y.avg[a,i] = auc(temp)
#     
#     temp.table = table(predicted = gen.avg$U_pred[i,], actual = gen.avg$U_orig)
#     TN.U.avg = temp.table[1,1] / sum(temp.table)
#     TP.U.avg = temp.table[2,2] / sum(temp.table)
#     FP.U.avg = temp.table[2,1] / sum(temp.table)
#     FN.U.avg = temp.table[1,2] / sum(temp.table)
#     spe.U.avg[a,i] = TN.U.avg /(TN.U.avg+FP.U.avg)
#     sen.U.avg[a,i] = TP.U.avg /(FN.U.avg+TP.U.avg)
#     ove.U.avg[a,i] = (TP.U.avg + TN.U.avg)
#     phi.U.avg[a,i] = (TP.U.avg * TN.U.avg + FP.U.avg * FN.U.avg) / sqrt((TP.U.avg + FN.U.avg) * (TN.U.avg + FP.U.avg) * (TP.U.avg + FP.U.avg) * (TN.U.avg + FN.U.avg))
#     temp = roc(gen.avg$U_orig, gen.avg$U_pred[i,])
#     auc.U.avg[a,i] = auc(temp)
#     print(c(a,i,auc.Y.avg[a,i],auc.U.avg[a,i]))
#   }
# }
# write.table(round(spe.Y.avg, 3), "RSUM/spe_Y_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.Y.avg, 3), "RSUM/sen_Y_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.Y.avg, 3), "RSUM/ove_Y_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.Y.avg, 3), "RSUM/phi_Y_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.Y.avg, 3), "RSUM/auc_Y_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(spe.U.avg, 3), "RSUM/spe_U_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.U.avg, 3), "RSUM/sen_U_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.U.avg, 3), "RSUM/ove_U_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.U.avg, 3), "RSUM/phi_U_avg.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.U.avg, 3), "RSUM/auc_U_avg.txt", row.names=FALSE, col.names=FALSE)
# 
# # --------------------------------------------------------------------------------------
# # Cutoff
# # --------------------------------------------------------------------------------------
# cutoff = seq(0.02, 0.5, by=0.02)
# auc.Y.roc = matrix(NA,nschool,length(cutoff)); sen.Y.roc = matrix(NA,nschool,length(cutoff)) 
# spe.Y.roc = matrix(NA,nschool,length(cutoff)); ove.Y.roc = matrix(NA,nschool,length(cutoff))
# auc.U.roc = matrix(NA,nschool,length(cutoff)); sen.U.roc = matrix(NA,nschool,length(cutoff))
# spe.U.roc = matrix(NA,nschool,length(cutoff)); ove.U.roc = matrix(NA,nschool,length(cutoff))
# phi.Y.roc = matrix(NA,nschool,length(cutoff)); phi.U.roc = matrix(NA,nschool,length(cutoff))
# 
# w_temp = array(0, dim=c(nschool, nitem, nitem))
# for(k in 1:nschool){
#   if(k < 10) fname1 = paste("RESULT/sum0",k,"_d1.log",sep="")
#   else fname1 = paste("RESULT/sum",k,"_d1.log",sep="")
#   temp = as.matrix(read.table(fname1))
#   index = 0
#   for(i in 2:nitem){
#     for(j in 1:(i-1)){
#       index = index + 1
#       w_temp[k,i,j] = temp[1,index]
#     }
#   }
#   w_temp[k,,] = w_temp[k,,] + t(w_temp[k,,])
# }
# 
# for(a in 1:nschool){
#   beta_temp = apply(sim_beta[a,,], 2, mean)
#   theta_temp = apply(sim_theta[a,,1:count[a]], 2, mean)
#   z_temp = array(0, dim=c(niter,count[a],ndim))
#   z_dist_temp = array(0, dim=c(niter,count[a],count[a]))
#   z_dist = matrix(0,count[a],count[a])
#   for(iter in 1:niter){
#     z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
#     z_dist_temp[iter,,] = as.matrix(dist(z_temp[iter,,]))
#   }
#   for(i in 1:count[a]){
#     for(j in 1:count[a]){
#       z_dist[i,j] = mean(z_dist_temp[,i,j])
#     }
#   }
#   
#   for(i in 1:length(cutoff)){
#     gen.roc = pp_roc(Y[a,,,], U[a,,,], beta_temp, theta_temp, z_dist, w_temp[a,,], count[a], nitem, ndim, cutoff[i])
#     temp.table = table(predicted = gen.roc$Y_pred, actual = gen.roc$Y_orig)
#     TN.Y.roc = temp.table[1,1] / sum(temp.table)
#     TP.Y.roc = temp.table[2,2] / sum(temp.table)
#     FP.Y.roc = temp.table[2,1] / sum(temp.table)
#     FN.Y.roc = temp.table[1,2] / sum(temp.table)
#     spe.Y.roc[a,i] = TN.Y.roc /(TN.Y.roc+FP.Y.roc)
#     sen.Y.roc[a,i] = TP.Y.roc /(FN.Y.roc+TP.Y.roc)
#     ove.Y.roc[a,i] = (TP.Y.roc + TN.Y.roc)
#     phi.Y.roc[a,i] = (TP.Y.roc * TN.Y.roc + FP.Y.roc * FN.Y.roc) / sqrt((TP.Y.roc + FN.Y.roc) * (TN.Y.roc + FP.Y.roc) * (TP.Y.roc + FP.Y.roc) * (TN.Y.roc + FN.Y.roc))
#     temp = roc(gen.roc$Y_orig, gen.roc$Y_pre)
#     auc.Y.roc[a,i] = auc(temp)
#     
#     temp.table = table(predicted = gen.roc$U_pred, actual = gen.roc$U_orig)
#     TN.U.roc = temp.table[1,1] / sum(temp.table)
#     TP.U.roc = temp.table[2,2] / sum(temp.table)
#     FP.U.roc = temp.table[2,1] / sum(temp.table)
#     FN.U.roc = temp.table[1,2] / sum(temp.table)
#     spe.U.roc[a,i] = TN.U.roc /(TN.U.roc+FP.U.roc)
#     sen.U.roc[a,i] = TP.U.roc /(FN.U.roc+TP.U.roc)
#     ove.U.roc[a,i] = (TP.U.roc + TN.U.roc)
#     phi.U.roc[a,i] = (TP.U.roc * TN.U.roc + FP.U.roc * FN.U.roc) / sqrt((TP.U.roc + FN.U.roc) * (TN.U.roc + FP.U.roc) * (TP.U.roc + FP.U.roc) * (TN.U.roc + FN.U.roc))
#     temp = roc(gen.roc$U_orig, gen.roc$U_pred)
#     auc.U.roc[a,i] = auc(temp)
#     
#     print(c(a,cutoff[i],spe.Y.roc[a,i],spe.U.roc[a,i],sen.Y.roc[a,i],sen.U.roc[a,i],phi.Y.roc[a,i],phi.U.roc[a,i],auc.Y.roc[a,i],auc.U.roc[a,i]))
#   }
# }
# 
# write.table(round(spe.Y.roc, 3), "RSUM/spe_Y_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.Y.roc, 3), "RSUM/sen_Y_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.Y.roc, 3), "RSUM/ove_Y_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.Y.roc, 3), "RSUM/phi_Y_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.Y.roc, 3), "RSUM/auc_Y_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(spe.U.roc, 3), "RSUM/spe_U_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(sen.U.roc, 3), "RSUM/sen_U_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(ove.U.roc, 3), "RSUM/ove_U_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(phi.U.roc, 3), "RSUM/phi_U_roc.txt", row.names=FALSE, col.names=FALSE)
# write.table(round(auc.U.roc, 3), "RSUM/auc_U_roc.txt", row.names=FALSE, col.names=FALSE)

auc.Y.final = rep(NA,nschool); sen.Y.final = rep(NA,nschool) 
spe.Y.final = rep(NA,nschool); ove.Y.final = rep(NA,nschool)
auc.U.final = rep(NA,nschool); sen.U.final = rep(NA,nschool)
spe.U.final = rep(NA,nschool); ove.U.final = rep(NA,nschool)
phi.Y.final = rep(NA,nschool); phi.U.final = rep(NA,nschool)

TN.Y.sum = rep(0,nschool); TN.U.sum = rep(0,nschool);
TP.Y.sum = rep(0,nschool); TP.U.sum = rep(0,nschool);
FN.Y.sum = rep(0,nschool); FN.U.sum = rep(0,nschool);
FP.Y.sum = rep(0,nschool); FP.U.sum = rep(0,nschool);

w_temp = array(0, dim=c(nschool, nitem, nitem))
for(k in 1:nschool){
  if(k < 10) fname1 = paste("RESULT/sum0",k,"_d1.log",sep="")
  else fname1 = paste("RESULT/sum",k,"_d1.log",sep="")
  temp = as.matrix(read.table(fname1))
  index = 0
  for(i in 2:nitem){
    for(j in 1:(i-1)){
      index = index + 1
      w_temp[k,i,j] = temp[1,index]
    }
  }
  w_temp[k,,] = w_temp[k,,] + t(w_temp[k,,])
}

for(a in 1:nschool){
  beta_temp = apply(sim_beta[a,,], 2, mean)
  theta_temp = apply(sim_theta[a,,1:count[a]], 2, mean)
  z_temp = array(0, dim=c(niter,count[a],ndim))
  z_dist_temp = array(0, dim=c(niter,count[a],count[a]))
  z_dist = matrix(0,count[a],count[a])
  for(iter in 1:niter){
    z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
    z_dist_temp[iter,,] = as.matrix(dist(z_temp[iter,,]))
  }
  for(i in 1:count[a]){
    for(j in 1:count[a]){
      z_dist[i,j] = mean(z_dist_temp[,i,j])
    }
  }
  
  lower_Y_orig = {}
  lower_Y_pred = {}
  for(k in 1:nitem){
    temp_mat_Y = Y[a,k,1:count[a],1:count[a]]
    lower_Y_orig = c(lower_Y_orig, temp_mat_Y[lower.tri(temp_mat_Y)])
    prob_mat_Y_pred = plogis(beta_temp[k] - z_dist)
    temp_mat_Y_pred = matrix(rbinom(count[a]*count[a],1,prob_mat_Y_pred),count[a],count[a])
    lower_Y_pred = c(lower_Y_pred, temp_mat_Y_pred[lower.tri(temp_mat_Y_pred)])
  }
  
  lower_U_orig = {}
  lower_U_pred = {}
  for(k in 1:count[a]){
    temp_mat_U = U[a,k,,]
    lower_U_orig = c(lower_U_orig, temp_mat_U[lower.tri(temp_mat_U)])
    prob_mat_U_pred = plogis(theta_temp[k] - w_temp[a,,])
    temp_mat_U_pred = matrix(rbinom(nitem*nitem,1,prob_mat_U_pred),nitem,nitem)
    lower_U_pred = c(lower_U_pred, temp_mat_U_pred[lower.tri(temp_mat_U_pred)])
  }
  
  temp.table = table(predicted = lower_Y_pred, actual = lower_Y_orig)
  TN.Y.final = temp.table[1,1] / sum(temp.table)
  TP.Y.final = temp.table[2,2] / sum(temp.table)
  FP.Y.final = temp.table[2,1] / sum(temp.table)
  FN.Y.final = temp.table[1,2] / sum(temp.table)
  TN.Y.sum[a] = TN.Y.sum[a] + TN.Y.final
  TP.Y.sum[a] = TP.Y.sum[a] + TP.Y.final
  FN.Y.sum[a] = FN.Y.sum[a] + FN.Y.final
  FP.Y.sum[a] = FP.Y.sum[a] + FP.Y.final
  spe.Y.final[a] = TN.Y.final /(TN.Y.final+FP.Y.final)
  sen.Y.final[a] = TP.Y.final /(FN.Y.final+TP.Y.final)
  ove.Y.final[a] = (TP.Y.final + TN.Y.final)
  phi.Y.final[a] = (TP.Y.final * TN.Y.final + FP.Y.final * FN.Y.final) / sqrt((TP.Y.final + FN.Y.final) * (TN.Y.final + FP.Y.final) * (TP.Y.final + FP.Y.final) * (TN.Y.final + FN.Y.final))
  temp = roc(lower_Y_orig, lower_Y_pred)
  auc.Y.final[a] = auc(temp)
  
  temp.table = table(predicted = lower_U_pred, actual = lower_U_orig)
  TN.U.final = temp.table[1,1] / sum(temp.table)
  TP.U.final = temp.table[2,2] / sum(temp.table)
  FP.U.final = temp.table[2,1] / sum(temp.table)
  FN.U.final = temp.table[1,2] / sum(temp.table)
  TN.U.sum[a] = TN.U.sum[a] + TN.U.final
  TP.U.sum[a] = TP.U.sum[a] + TP.U.final
  FN.U.sum[a] = FN.U.sum[a] + FN.U.final
  FP.U.sum[a] = FP.U.sum[a] + FP.U.final
  spe.U.final[a] = TN.U.final /(TN.U.final+FP.U.final)
  sen.U.final[a] = TP.U.final /(FN.U.final+TP.U.final)
  ove.U.final[a] = (TP.U.final + TN.U.final)
  phi.U.final[a] = (TP.U.final * TN.U.final + FP.U.final * FN.U.final) / sqrt((TP.U.final + FN.U.final) * (TN.U.final + FP.U.final) * (TP.U.final + FP.U.final) * (TN.U.final + FN.U.final))
  temp = roc(lower_U_orig, lower_U_pred)
  auc.U.final[a] = auc(temp)
  
  print(c(a,round(spe.Y.final[a],3),round(spe.U.final[a],3),round(sen.Y.final[a],3),round(sen.U.final[a],3),round(phi.Y.final[a],3),round(phi.U.final[a],3),round(auc.Y.final[a],3),round(auc.U.final[a],3)))
}

final.Y = cbind(sen.Y.final,spe.Y.final,ove.Y.final,phi.Y.final,auc.Y.final)
final.U = cbind(sen.U.final,spe.U.final,ove.U.final,phi.U.final,auc.U.final)
final.Y.mean = apply(final.Y,2,mean)
final.U.mean = apply(final.U,2,mean)
final.mean = rbind(final.Y.mean,final.U.mean)
colnames(final.Y) = c("Sensitivity", "Specificity", "Overall", "MCC", "AUC")
colnames(final.U) = c("Sensitivity", "Specificity", "Overall", "MCC", "AUC")
colnames(final.mean) = c("Sensitivity", "Specificity", "Overall", "MCC", "AUC")
write.table(round(final.Y,3),"RSUM/final_Y.txt",row.names=FALSE)
write.table(round(final.U,3),"RSUM/final_U.txt",row.names=FALSE)
write.table(round(final.mean,3),"RSUM/final_mean.txt",row.names=FALSE)



#---------------------------------------------------------------------------------------------------------------
# Posterior Predictive Distribution
#---------------------------------------------------------------------------------------------------------------

nsim = 1000
y.sim = array(NA,dim=c(nsim,nschool,nitem))
u.sim = array(NA,dim=c(nsim,nschool,nmax))
y.sum = matrix(NA,nschool,nitem)
u.sum = matrix(NA,nschool,nmax)
y.pec = matrix(NA,nschool,nitem)
u.pec = matrix(NA,nschool,nmax)

w_temp = array(0, dim=c(nschool, nitem, nitem))
for(k in 1:nschool){
  if(k < 10) fname1 = paste("RESULT/sum0",k,"_d1.log",sep="")
  else fname1 = paste("RESULT/sum",k,"_d1.log",sep="")
  temp = as.matrix(read.table(fname1))
  index = 0
  for(i in 2:nitem){
    for(j in 1:(i-1)){
      index = index + 1
      w_temp[k,i,j] = temp[1,index]
    }
  }
  w_temp[k,,] = w_temp[k,,] + t(w_temp[k,,])
}

for(a in 1:nschool){
  beta_temp = apply(sim_beta[a,,], 2, mean)
  theta_temp = apply(sim_theta[a,,1:count[a]], 2, mean)
  z_temp = array(0, dim=c(niter,count[a],ndim))
  z_dist_temp = array(0, dim=c(niter,count[a],count[a]))
  z_dist = matrix(0,count[a],count[a])
  for(iter in 1:niter){
    z_temp[iter,,] = matrix(sim_z[a,iter,1:(count[a]*ndim)], ncol=ndim, byrow=TRUE)
    z_dist_temp[iter,,] = as.matrix(dist(z_temp[iter,,]))
  }
  for(i in 1:count[a]){
    for(j in 1:count[a]){
      z_dist[i,j] = mean(z_dist_temp[,i,j])
    }
  }
  
  for(k in 1:nitem){
    temp_mat_Y = Y[a,k,1:count[a],1:count[a]]
    y.sum[a,k] = sum(temp_mat_Y[lower.tri(temp_mat_Y)])
  }
  
  for(k in 1:count[a]){
    temp_mat_U = U[a,k,,]
    u.sum[a,k] = sum(temp_mat_U[lower.tri(temp_mat_U)])
  }
  
  for(b in 1:nsim){
    for(k in 1:nitem){
      prob_mat_Y_pred = plogis(beta_temp[k] - z_dist)
      temp_mat_Y = matrix(rbinom(count[a]*count[a],1,prob_mat_Y_pred),count[a],count[a])
      y.sim[b,a,k] = sum(temp_mat_Y[lower.tri(temp_mat_Y)])
    }
    
    for(k in 1:count[a]){
      prob_mat_U_pred = plogis(theta_temp[k] - w_temp[a,,])
      temp_mat_U = matrix(rbinom(nitem*nitem,1,prob_mat_U_pred),nitem,nitem)
      u.sim[b,a,k] = sum(temp_mat_U[lower.tri(temp_mat_U)])
    }
    print(c(a,b))
  }
}

for(a in 1:nschool){
  for(k in 1:nitem) y.pec[a,k] = ecdf(y.sim[,a,k])(y.sum[a,k])
  for(k in 1:count[a]) u.pec[a,k] = ecdf(u.sim[,a,k])(u.sum[a,k])
}
write.table(y.pec,"pp_y.txt",row.names=FALSE,col.names=FALSE)
write.table(u.pec,"pp_u.txt",row.names=FALSE,col.names=FALSE)

index = 0
theta_sum = matrix(0, nmax, nschool)
for(i in 1:nschool){
  theta_sum[(1:count[i]),i] = theta_result[(index+1):(index+count[i])]
  index = index + count[i]
}

colors = rep(1,62)
colors[c(19,28)] = "#FF0000"
colors[c(10)] = "#3399FF"

pdf("lsrm_theta.pdf")
x.axis = c(1:nschool)
y.axis = x.axis
plot(x.axis,y.axis,ylim=c(-6.00,6.00),xlab="school",ylab="random effect",type="n")
for(i in 1:nschool){
  lines(c(i,i),c(min(theta_sum[1:count[i],i]),max(theta_sum[1:count[i],i])),lwd=2, col=colors[i])
  points(i,mean(theta_sum[1:count[i],i]),pch=20,col=4)
} 
for(i in 1:6) abline(v=i*10, lty=3, col=2)
dev.off()

#---------------------------------------------------------------------------------------------------------------
# Save Computation Result
#---------------------------------------------------------------------------------------------------------------
save.image("GEPS-MODEL1-RUN1-ADD.RData")
