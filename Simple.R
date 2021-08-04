a = c(1,1,1,0,0,0)
b = c(0,0,0,1,1,1)
aa = rep(a,50)
aaa = matrix(aa,ncol=6,byrow=TRUE)
bb = rep(b,50)
bbb = matrix(bb,ncol=6,byrow=TRUE)
x = rbind(aaa,bbb)

library(nirm)
output = itor_nirm(x, nrow(x), ncol(x), ndim=2, niter=30000, nburn=5000, nthin=5, nprint=100,
                   jump_beta=0.2, jump_theta=1.0, jump_z=0.1, jump_w=0.1,
                   pr_mean_beta = 0.0, pr_sd_beta = 10.0, pr_mean_theta = 0.0, pr_sd_theta = 10.0,
                   pr_mean_w = 0.0, prior_a = 0.001, prior_b = 0.001, option = TRUE, similarity = FALSE, cores = 1)

pdf("simple.pdf")
plot(output$z.estimate,cex.axis=1,pch="",xlab="",ylab="",xlim=c(-2,5),ylim=c(-3,5))
points(output$z.estimate,pch=20,col=2,cex=2)
text(output$w.estimate,labels=1:ncol(output$beta),cex=1.5,font=2,col=4)
dev.off()

pdf("simple_samp.pdf")
plot(output$z.estimate,cex.axis=1,pch="",xlab="",ylab="",xlim=c(-2,5),ylim=c(-3,5))
points(output$z.estimate,pch=20,col=2,cex=2)
dev.off()

pdf("simple_item.pdf")
plot(output$z.estimate,cex.axis=1,pch="",xlab="",ylab="",xlim=c(-2,5),ylim=c(-3,5))
text(output$w.estimate,labels=1:ncol(output$beta),cex=2,font=2,col=4)
dev.off()

save.image("simple.RData")