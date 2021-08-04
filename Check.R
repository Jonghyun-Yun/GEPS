spe.Y.roc = as.matrix(read.table("RSUM/spe_Y_roc.txt"))
sen.Y.roc = as.matrix(read.table("RSUM/sen_Y_roc.txt"))
ove.Y.roc = as.matrix(read.table("RSUM/ove_Y_roc.txt"))
phi.Y.roc = as.matrix(read.table("RSUM/phi_Y_roc.txt"))
auc.Y.roc = as.matrix(read.table("RSUM/auc_Y_roc.txt"))
spe.U.roc = as.matrix(read.table("RSUM/spe_U_roc.txt"))
sen.U.roc = as.matrix(read.table("RSUM/sen_U_roc.txt"))
ove.U.roc = as.matrix(read.table("RSUM/ove_U_roc.txt"))
phi.U.roc = as.matrix(read.table("RSUM/phi_U_roc.txt"))
auc.U.roc = as.matrix(read.table("RSUM/auc_U_roc.txt"))

specificity.y = round(apply(spe.Y.roc,2,mean),3)
specificity.u = round(apply(spe.U.roc,2,mean),3)
sensitivity.y = round(apply(sen.Y.roc,2,mean),3)
sensitivity.u = round(apply(sen.U.roc,2,mean),3)
overall.y = round(apply(ove.Y.roc,2,mean),3)
overall.u = round(apply(ove.U.roc,2,mean),3)
mcc.y = round(apply(phi.Y.roc,2,mean),3)
mcc.u = round(apply(phi.U.roc,2,mean),3)
auc.y = round(apply(auc.Y.roc,2,mean),3)
auc.u = round(apply(auc.U.roc,2,mean),3)
performance.table.y = rbind(overall.y,sensitivity.y,specificity.y,mcc.y,auc.y)
performance.table.u = rbind(overall.u,sensitivity.u,specificity.u,mcc.u,auc.u)
colnames(performance.table.y) = seq(0.02,0.5,by=0.02)
colnames(performance.table.u) = seq(0.02,0.5,by=0.02)

nitem = 72;
nschool = 62;
nmax = 81;
count = scan("DATA/count.txt")
Y = array(0, dim=c(nschool,nitem,nmax,nmax))
U = array(0, dim=c(nschool,nmax,nitem,nitem))

table.Y = matrix(0,nschool,2)
table.U = matrix(0,nschool,2)
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
}

table.Y = matrix(0,nschool,2)
table.U = matrix(0,nschool,2)