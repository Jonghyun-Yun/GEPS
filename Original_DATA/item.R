# ---------------------------------------------------------------------------------------------------------
# High School
# ---------------------------------------------------------------------------------------------------------
nschool = 62
item = c(1:33,42:68,71:72)
nitem = length(c(1:33,42:68,71:72))
for(i in 1:nschool){
  if(i < 10) fopen = paste("DATA/item0",i,".txt",sep="")
  else fopen = paste("DATA/item",i,".txt",sep="")
  
  X = as.matrix(read.table(fopen))
  X = X[,c(1:33,42:68,71:72)]
  
  if(i < 10) fopen = paste("y3_high/item0",i,".txt",sep="")
  else fopen = paste("y3_high/item",i,".txt",sep="")
  write.table(X,fopen,row.names=FALSE,col.names=FALSE)
}
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------
# Middle School
# ---------------------------------------------------------------------------------------------------------
library(foreign)
nschool = 63
middle = read.spss("y3_middle.sav",to.data.frame=TRUE)
middle.data = middle[!is.na(middle$Y3M_CLASS),]

sid.middle = as.numeric(middle.data$SCHID)-200

mental = cbind(as.numeric(middle.data$Y3M_ST17_1) > 3, as.numeric(middle.data$Y3M_ST17_2) > 3,
               as.numeric(middle.data$Y3M_ST17_3) > 3, as.numeric(middle.data$Y3M_ST17_4) > 3,
               as.numeric(middle.data$Y3M_ST17_5) > 3, as.numeric(middle.data$Y3M_ST17_6) > 3) * 1
new.middle = mental # 1 - 6

citizen = cbind(as.numeric((middle.data$Y3M_ST18_1)) > 3, as.numeric((middle.data$Y3M_ST18_2)) > 3,
                as.numeric((middle.data$Y3M_ST18_3)) > 3, as.numeric((middle.data$Y3M_ST18_4)) > 3,
                as.numeric((middle.data$Y3M_ST18_5)) > 3, as.numeric((middle.data$Y3M_ST18_7)) > 3,
                as.numeric((middle.data$Y3M_ST18_8)) > 3, as.numeric((middle.data$Y3M_ST18_9)) > 3,
                as.numeric((middle.data$Y3M_ST18_10)) > 3, as.numeric((middle.data$Y3M_ST18_11)) > 3,
                as.numeric((middle.data$Y3M_ST18_12)) > 3, as.numeric((middle.data$Y3M_ST18_13)) > 3,
                as.numeric((middle.data$Y3M_ST18_14)) > 3, as.numeric((middle.data$Y3M_ST18_15)) > 3,
                as.numeric((middle.data$Y3M_ST18_16)) > 3, as.numeric((middle.data$Y3M_ST18_17)) > 3) * 1
new.middle = cbind(new.middle,citizen) # 7 - 22

efficacy = cbind(as.numeric((middle.data$Y3M_ST19_1)) > 3, as.numeric((middle.data$Y3M_ST19_2)) > 3,
                 as.numeric((middle.data$Y3M_ST19_3)) > 3, as.numeric((middle.data$Y3M_ST19_4)) > 3,
                 as.numeric((middle.data$Y3M_ST19_5)) > 3, as.numeric((middle.data$Y3M_ST19_8)) > 3,
                 as.numeric((middle.data$Y3M_ST19_9)) > 3, as.numeric((middle.data$Y3M_ST19_10)) > 3) * 1
new.middle = cbind(new.middle,efficacy) # 23 - 30

belief = cbind(as.numeric((middle.data$Y3M_ST22_1)) > 3, as.numeric((middle.data$Y3M_ST22_2)) > 3,
               as.numeric((middle.data$Y3M_ST22_3)) > 3) * 1
new.middle = cbind(new.middle,belief) # 31 - 33
full.middle = new.middle

learning = cbind(as.numeric((middle.data$Y3M_ST25_1)) > 3, as.numeric((middle.data$Y3M_ST25_2)) > 3,
                 as.numeric((middle.data$Y3M_ST25_3)) > 3, as.numeric((middle.data$Y3M_ST25_4)) > 3,
                 as.numeric((middle.data$Y3M_ST25_5)) > 3, as.numeric((middle.data$Y3M_ST25_6)) > 3,
                 as.numeric((middle.data$Y3M_ST25_7)) > 3, as.numeric((middle.data$Y3M_ST25_8)) > 3) * 1
full.middle = cbind(full.middle,learning) # 34 - 41

stress = cbind(as.numeric((middle.data$Y3M_ST27_1)) > 3, as.numeric((middle.data$Y3M_ST27_2)) > 3,
               as.numeric((middle.data$Y3M_ST27_3)) > 3, as.numeric((middle.data$Y3M_ST27_4)) > 3,
               as.numeric((middle.data$Y3M_ST27_5)) > 3, as.numeric((middle.data$Y3M_ST27_6)) > 3,
               as.numeric((middle.data$Y3M_ST27_7)) > 3) * 1
new.middle = cbind(new.middle,stress) # 34 - 40
full.middle = cbind(full.middle,stress) # 42 - 48

friends = cbind(as.numeric((middle.data$Y3M_ST44_1)) > 3, as.numeric((middle.data$Y3M_ST44_2)) > 3,
                as.numeric((middle.data$Y3M_ST44_3)) > 3, as.numeric((middle.data$Y3M_ST44_4)) > 3,
                as.numeric((middle.data$Y3M_ST44_5)) > 3, as.numeric((middle.data$Y3M_ST44_6)) > 3) * 1
new.middle = cbind(new.middle,friends) # 41 - 46
full.middle = cbind(full.middle,friends) # 49 - 54

esteem = cbind(as.numeric((middle.data$Y3M_ST20_1)) > 3, as.numeric((middle.data$Y3M_ST20_2)) > 3,
               as.numeric((middle.data$Y3M_ST20_3)) > 3, as.numeric((middle.data$Y3M_ST20_7)) > 3) * 1
new.middle = cbind(new.middle,esteem) # 47 - 50
full.middle = cbind(full.middle,esteem) # 55 - 58

academic = cbind(as.numeric((middle.data$Y3M_ST26_1)) > 3, as.numeric((middle.data$Y3M_ST26_6)) > 3,
                 as.numeric((middle.data$Y3M_ST26_7)) > 3, as.numeric((middle.data$Y3M_ST26_8)) > 3,
                 as.numeric((middle.data$Y3M_ST26_9)) > 3, as.numeric((middle.data$Y3M_ST26_10)) > 3,
                 as.numeric((middle.data$Y3M_ST26_11)) > 3, as.numeric((middle.data$Y3M_ST26_12)) > 3,
                 as.numeric((middle.data$Y3M_ST26_13)) > 3, as.numeric((middle.data$Y3M_ST26_14)) > 3) * 1
new.middle = cbind(new.middle,academic) # 51 - 60
full.middle = cbind(full.middle,academic) # 59 - 68

appear = (as.numeric((middle.data$Y3M_ST19_6)) > 3) * 1 + (as.numeric((middle.data$Y3M_ST19_7)) > 3) * 1
appear = (appear > 0) * 1

positive = (as.numeric((middle.data$Y3M_ST20_4)) > 3) * 1 + (as.numeric((middle.data$Y3M_ST20_5)) > 3) * 1
positive = (positive > 0) * 1
new.middle = cbind(new.middle, appear, positive) # 61 - 62
full.middle = cbind(full.middle, appear, positive) # 69 - 70

new.middle[is.na(new.middle)] = 0
full.middle[is.na(full.middle)] = 0

for(i in 1:nschool){
  if(i < 10) fopen1 = paste("y3_middle1/item0",i,".txt",sep="")
  else fopen1 = paste("y3_middle1/item",i,".txt",sep="")
  
  if(i < 10) fopen2 = paste("y3_middle2/item0",i,".txt",sep="")
  else fopen2 = paste("y3_middle2/item",i,".txt",sep="")
  
  temp1 = new.middle[sid.middle==i,]
  temp2 = full.middle[sid.middle==i,]
  
  write.table(temp1,fopen1,row.names=FALSE,col.names=FALSE)
  write.table(temp2,fopen2,row.names=FALSE,col.names=FALSE)
}

smiddle = read.spss("y3_school_middle.sav",to.data.frame=TRUE)
renov = as.numeric(smiddle$Y3M_SCH1_1)
renov[renov==2] = 0
write.table(renov,"y3_middle1/renov.txt",row.names=FALSE,col.names=FALSE)
write.table(renov,"y3_middle2/renov.txt",row.names=FALSE,col.names=FALSE)
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------
# Elementary School
# ---------------------------------------------------------------------------------------------------------
library(foreign)
element = read.spss("y3_elementary.sav",to.data.frame=TRUE)
element.data = element[!is.na(element$Y3E_CLASS),]

sid.element = as.numeric(element.data$SCHID)-100
nstudent = table(sid.element)
school = as.numeric(which(nstudent > 20))
nschool = length(school)
element.newdata = element.data[sid.element == school[1],]
sid.newelement = sid.element[sid.element == school[1]]
for(i in 2:nschool){
  element.newdata = rbind(element.newdata,element.data[sid.element == school[i],])
  sid.newelement = c(sid.newelement,sid.element[sid.element == school[i]])
}

mental = cbind(as.numeric(element.newdata$Y3E_ST16_1) > 3, as.numeric(element.newdata$Y3E_ST16_2) > 3,
               as.numeric(element.newdata$Y3E_ST16_3) > 3, as.numeric(element.newdata$Y3E_ST16_4) > 3,
               as.numeric(element.newdata$Y3E_ST16_5) > 3, as.numeric(element.newdata$Y3E_ST16_6) > 3) * 1
new.element = mental # 1 - 6

citizen = cbind(as.numeric((element.newdata$Y3E_ST17_1)) > 3, as.numeric((element.newdata$Y3E_ST17_2)) > 3,
                as.numeric((element.newdata$Y3E_ST17_3)) > 3, as.numeric((element.newdata$Y3E_ST17_4)) > 3,
                as.numeric((element.newdata$Y3E_ST17_5)) > 3, as.numeric((element.newdata$Y3E_ST17_7)) > 3,
                as.numeric((element.newdata$Y3E_ST17_8)) > 3, as.numeric((element.newdata$Y3E_ST17_9)) > 3,
                as.numeric((element.newdata$Y3E_ST17_10)) > 3, as.numeric((element.newdata$Y3E_ST17_11)) > 3,
                as.numeric((element.newdata$Y3E_ST17_12)) > 3, as.numeric((element.newdata$Y3E_ST17_13)) > 3,
                as.numeric((element.newdata$Y3E_ST17_14)) > 3, as.numeric((element.newdata$Y3E_ST17_15)) > 3,
                as.numeric((element.newdata$Y3E_ST17_16)) > 3, as.numeric((element.newdata$Y3E_ST17_17)) > 3) * 1
new.element = cbind(new.element,citizen) # 7 - 22

efficacy = cbind(as.numeric((element.newdata$Y3E_ST18_1)) > 3, as.numeric((element.newdata$Y3E_ST18_2)) > 3,
                 as.numeric((element.newdata$Y3E_ST18_3)) > 3, as.numeric((element.newdata$Y3E_ST18_4)) > 3,
                 as.numeric((element.newdata$Y3E_ST18_5)) > 3, as.numeric((element.newdata$Y3E_ST18_8)) > 3,
                 as.numeric((element.newdata$Y3E_ST18_9)) > 3, as.numeric((element.newdata$Y3E_ST18_10)) > 3) * 1
new.element = cbind(new.element,efficacy) # 23 - 30

belief = cbind(as.numeric((element.newdata$Y3E_ST21_1)) > 3, as.numeric((element.newdata$Y3E_ST21_2)) > 3,
               as.numeric((element.newdata$Y3E_ST21_3)) > 3) * 1
new.element = cbind(new.element,belief) # 31 - 33

stress = cbind(as.numeric((element.newdata$Y3E_ST24_1)) > 3, as.numeric((element.newdata$Y3E_ST24_2)) > 3,
               as.numeric((element.newdata$Y3E_ST24_3)) > 3, as.numeric((element.newdata$Y3E_ST24_4)) > 3,
               as.numeric((element.newdata$Y3E_ST24_5)) > 3, as.numeric((element.newdata$Y3E_ST24_6)) > 3,
               as.numeric((element.newdata$Y3E_ST24_7)) > 3) * 1
new.element = cbind(new.element,stress) # 34 - 40

friends = cbind(as.numeric((element.newdata$Y3E_ST36_1)) > 3, as.numeric((element.newdata$Y3E_ST36_2)) > 3,
                as.numeric((element.newdata$Y3E_ST36_3)) > 3, as.numeric((element.newdata$Y3E_ST36_4)) > 3,
                as.numeric((element.newdata$Y3E_ST36_5)) > 3, as.numeric((element.newdata$Y3E_ST36_6)) > 3) * 1
new.element = cbind(new.element,friends) # 41 - 46

esteem = cbind(as.numeric((element.newdata$Y3E_ST19_1)) > 3, as.numeric((element.newdata$Y3E_ST19_2)) > 3,
               as.numeric((element.newdata$Y3E_ST19_3)) > 3, as.numeric((element.newdata$Y3E_ST19_7)) > 3) * 1
new.element = cbind(new.element,esteem) # 47 - 50

academic = cbind(as.numeric((element.newdata$Y3E_ST23_1)) > 3, as.numeric((element.newdata$Y3E_ST23_6)) > 3,
                 as.numeric((element.newdata$Y3E_ST23_7)) > 3, as.numeric((element.newdata$Y3E_ST23_8)) > 3,
                 as.numeric((element.newdata$Y3E_ST23_9)) > 3, as.numeric((element.newdata$Y3E_ST23_10)) > 3,
                 as.numeric((element.newdata$Y3E_ST23_11)) > 3, as.numeric((element.newdata$Y3E_ST23_12)) > 3,
                 as.numeric((element.newdata$Y3E_ST23_13)) > 3, as.numeric((element.newdata$Y3E_ST23_14)) > 3) * 1
new.element = cbind(new.element,academic) # 51 - 60

appear = (as.numeric((element.newdata$Y3E_ST18_6)) > 3) * 1 + (as.numeric((element.newdata$Y3E_ST18_7)) > 3) * 1
appear = (appear > 0) * 1

positive = (as.numeric((element.newdata$Y3E_ST19_4)) > 3) * 1 + (as.numeric((element.newdata$Y3E_ST19_5)) > 3) * 1
positive = (positive > 0) * 1
new.element = cbind(new.element, appear, positive) # 61 - 62

new.element[is.na(new.element)] = 0

for(i in 1:nschool){
  if(i < 10) fopen = paste("y3_elementary/item0",i,".txt",sep="")
  else fopen = paste("y3_elementary/item",i,".txt",sep="")

  temp = new.element[sid.newelement == school[i],]
  
  write.table(temp,fopen,row.names=FALSE,col.names=FALSE)
}
selement = read.spss("y3_school_elementary.sav",to.data.frame=TRUE)
renov = as.numeric(selement$Y3E_SCH1_1)
renov[renov==2] = 0
renov = renov[school]
table(renov)
write.table(renov,"y3_elementary/renov.txt",row.names=FALSE,col.names=FALSE)

rm(list = ls())
