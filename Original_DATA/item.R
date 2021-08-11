rev_code <- function(x) {
  if(x < 1 || x > 5) stop("input: out of range.")
## 1 2 3 4 5 -> 5 4 3 2 1
  return(abs(x-6))
}

## ---------------------------------------------------------------------------------------------------------
## High School
## ---------------------------------------------------------------------------------------------------------
library(foreign)
nschool <- 62 # why not 63 schools??
high <- read.spss("y3_high.sav", to.data.frame = TRUE)
high.data <- high[!is.na(high$Y3H_CLASS), ]

sid.high <- as.numeric(high.data$SCHID) - 300

mental <- cbind(
  high.data$Y3H_ST18_1, high.data$Y3H_ST18_2,
  high.data$Y3H_ST18_3, high.data$Y3H_ST18_4,
  high.data$Y3H_ST18_5, high.data$Y3H_ST18_6
) * 1
full.high <- mental # 1 - 6

citizen <- cbind(
  high.data$Y3H_ST19_1, high.data$Y3H_ST19_2,
  high.data$Y3H_ST19_3, high.data$Y3H_ST19_4,
  high.data$Y3H_ST19_5, high.data$Y3H_ST19_7,
  high.data$Y3H_ST19_8, high.data$Y3H_ST19_9,
  high.data$Y3H_ST19_10, high.data$Y3H_ST19_11,
  high.data$Y3H_ST19_12, high.data$Y3H_ST19_13,
  high.data$Y3H_ST19_14, high.data$Y3H_ST19_15,
  high.data$Y3H_ST19_16, high.data$Y3H_ST19_17
) * 1
full.high <- cbind(new.high, citizen) # 7 - 22

efficacy <- cbind(
  high.data$Y3H_ST20_1, high.data$Y3H_ST20_2,
  high.data$Y3H_ST20_3, high.data$Y3H_ST20_4,
  high.data$Y3H_ST20_5, high.data$Y3H_ST20_8,
  high.data$Y3H_ST20_9, high.data$Y3H_ST20_10
) * 1
full.high <- cbind(new.high, efficacy) # 23 - 30

belief <- cbind(
  high.data$Y3H_ST23_1, high.data$Y3H_ST23_2,
  high.data$Y3H_ST23_3
) * 1
full.high <- cbind(new.high, belief) # 31 - 33

learning <- cbind(
  high.data$Y3H_ST26_1, high.data$Y3H_ST26_2,
  high.data$Y3H_ST26_3, high.data$Y3H_ST26_4,
  high.data$Y3H_ST26_5, high.data$Y3H_ST26_6,
  high.data$Y3H_ST26_7, high.data$Y3H_ST26_8
) * 1
full.high <- cbind(full.high, learning) # 34 - 41

stress <- cbind(
  high.data$Y3H_ST28_1, high.data$Y3H_ST28_2,
  high.data$Y3H_ST28_3, high.data$Y3H_ST28_4,
  high.data$Y3H_ST28_5, high.data$Y3H_ST28_6,
  high.data$Y3H_ST28_7,
  high.data$Y3H_ST28_8,
  high.data$Y3H_ST28_9
) * 1
full.high <- cbind(full.high, stress) # 42 - 48

friends <- cbind(
  high.data$Y3H_ST46_1, high.data$Y3H_ST46_2,
  high.data$Y3H_ST46_3, high.data$Y3H_ST46_4,
  high.data$Y3H_ST46_5, high.data$Y3H_ST46_6
) * 1
full.high <- cbind(full.high, friends) # 49 - 54

esteem <- cbind(
  high.data$Y3H_ST21_1, high.data$Y3H_ST21_2,
  high.data$Y3H_ST21_3, high.data$Y3H_ST21_7
) * 1
full.high <- cbind(full.high, esteem) # 55 - 58

academic <- cbind(
  high.data$Y3H_ST27_1, high.data$Y3H_ST27_6,
  high.data$Y3H_ST27_7, high.data$Y3H_ST27_8,
  high.data$Y3H_ST27_9, high.data$Y3H_ST27_10,
  high.data$Y3H_ST27_11, high.data$Y3H_ST27_12,
  high.data$Y3H_ST27_13, high.data$Y3H_ST27_14
) * 1
full.high <- cbind(full.high, academic) # 59 - 68

appear <- as.numeric(high.data$Y3H_ST20_6) * 1 + as.numeric(high.data$Y3H_ST20_7) * 1
appear <- appear / 2

positive <- as.numeric(high.data$Y3H_ST21_4) * 1 + as.numeric(high.data$Y3H_ST21_5) * 1
positive <- positive / 2
full.high <- cbind(full.high, appear, positive) # 69 - 70

full.high[is.na(full.high)] <- 0

## reverse coding
full.neg_item <-c(1:6, 13:14, 20:22, 31:33, 42:48, 58:68)

for (ii in full.neg_item) {
  full.high[, ii] <- rev_code(full.high[, ii])
}

## dichotomizing
full.high = 1 * (full.high > 3)

## write to files
for (i in 1:nschool) {
  if (i < 10) {
    fopen <- paste("y3_high/item0", i, ".txt", sep = "")
  } else {
    fopen <- paste("y3_high/item", i, ".txt", sep = "")
  }

  temp <- full.high[sid.high == i, ]

  write.table(temp, fopen, row.names = FALSE, col.names = FALSE)
}

## i don't have this file
shigh <- read.spss("y3_school_high.sav", to.data.frame = TRUE)
renov <- as.numeric(shigh$Y3H_SCH1_1)
renov[renov == 2] <- 0
write.table(renov, "y3_high/renov.txt", row.names = FALSE, col.names = FALSE)
rm(list = ls())

## ---------------------------------------------------------------------------------------------------------
## Middle School
## ---------------------------------------------------------------------------------------------------------
library(foreign)
nschool <- 63
middle <- read.spss("y3_middle.sav", to.data.frame = TRUE)
middle.data <- middle[!is.na(middle$Y3M_CLASS), ]

sid.middle <- as.numeric(middle.data$SCHID) - 200

mental <- cbind(
  middle.data$Y3M_ST17_1, middle.data$Y3M_ST17_2,
  middle.data$Y3M_ST17_3, middle.data$Y3M_ST17_4,
  middle.data$Y3M_ST17_5, middle.data$Y3M_ST17_6
) * 1
full.middle <- mental # 1 - 6

citizen <- cbind(
  middle.data$Y3M_ST18_1, middle.data$Y3M_ST18_2,
  middle.data$Y3M_ST18_3, middle.data$Y3M_ST18_4,
  middle.data$Y3M_ST18_5, middle.data$Y3M_ST18_7,
  middle.data$Y3M_ST18_8, middle.data$Y3M_ST18_9,
  middle.data$Y3M_ST18_10, middle.data$Y3M_ST18_11,
  middle.data$Y3M_ST18_12, middle.data$Y3M_ST18_13,
  middle.data$Y3M_ST18_14, middle.data$Y3M_ST18_15,
  middle.data$Y3M_ST18_16, middle.data$Y3M_ST18_17
) * 1
full.middle <- cbind(full.middle, citizen) # 7 - 22

efficacy <- cbind(
  middle.data$Y3M_ST19_1, middle.data$Y3M_ST19_2,
  middle.data$Y3M_ST19_3, middle.data$Y3M_ST19_4,
  middle.data$Y3M_ST19_5, middle.data$Y3M_ST19_8,
  middle.data$Y3M_ST19_9, middle.data$Y3M_ST19_10
) * 1
full.middle <- cbind(full.middle, efficacy) # 23 - 30

belief <- cbind(
  middle.data$Y3M_ST22_1, middle.data$Y3M_ST22_2,
  middle.data$Y3M_ST22_3
) * 1
full.middle <- cbind(full.middle, belief) # 31 - 33

learning <- cbind(
  middle.data$Y3M_ST25_1, middle.data$Y3M_ST25_2,
  middle.data$Y3M_ST25_3, middle.data$Y3M_ST25_4,
  middle.data$Y3M_ST25_5, middle.data$Y3M_ST25_6,
  middle.data$Y3M_ST25_7, middle.data$Y3M_ST25_8
) * 1
full.middle <- cbind(full.middle, learning) # 34 - 41

stress <- cbind(
  middle.data$Y3M_ST27_1, middle.data$Y3M_ST27_2,
  middle.data$Y3M_ST27_3, middle.data$Y3M_ST27_4,
  middle.data$Y3M_ST27_5, middle.data$Y3M_ST27_6,
  middle.data$Y3M_ST27_7
) * 1
full.middle <- cbind(full.middle, stress) # 42 - 48

friends <- cbind(
  middle.data$Y3M_ST44_1, middle.data$Y3M_ST44_2,
  middle.data$Y3M_ST44_3, middle.data$Y3M_ST44_4,
  middle.data$Y3M_ST44_5, middle.data$Y3M_ST44_6
) * 1
full.middle <- cbind(full.middle, friends) # 49 - 54

esteem <- cbind(
  middle.data$Y3M_ST20_1, middle.data$Y3M_ST20_2,
  middle.data$Y3M_ST20_3, middle.data$Y3M_ST20_7
) * 1
full.middle <- cbind(full.middle, esteem) # 55 - 58

academic <- cbind(
  middle.data$Y3M_ST26_1, middle.data$Y3M_ST26_6,
  middle.data$Y3M_ST26_7, middle.data$Y3M_ST26_8,
  middle.data$Y3M_ST26_9, middle.data$Y3M_ST26_10,
  middle.data$Y3M_ST26_11, middle.data$Y3M_ST26_12,
  middle.data$Y3M_ST26_13, middle.data$Y3M_ST26_14
) * 1
full.middle <- cbind(full.middle, academic) # 59 - 68

appear <- apply(cbind(
  as.numeric(high.data$Y3H_ST19_6),
  as.numeric(high.data$Y3H_ST19_7)
), 1, max)
positive <- apply(cbind(
  as.numeric(high.data$Y3H_ST20_4),
  as.numeric(high.data$Y3H_ST20_5)
), 1, max)
full.middle <- cbind(full.middle, appear, positive) # 69 - 70

full.middle[is.na(full.middle)] <- 0

## reverse coding
full.neg_item <- c(1:6, 13:14, 20:22, 31:33, 42:48, 58:68)

for (ii in full.neg_item) {
  full.middle[, ii] <- rev_code(full.middle[, ii])
}

## dichotomizing
full.middle <- 1 * (full.middle > 3)

## write to files
for (i in 1:nschool) {
  if (i < 10) {
    fopen <- paste("y3_middle/item0", i, ".txt", sep = "")
  } else {
    fopen <- paste("y3_middle/item", i, ".txt", sep = "")
  }

  temp <- full.middle[sid.middle == i, ]

  write.table(temp, fopen, row.names = FALSE, col.names = FALSE)
}

# i don't have this file
smiddle <- read.spss("y3_school_middle.sav", to.data.frame = TRUE)
renov <- as.numeric(smiddle$Y3M_SCH1_1)
renov[renov == 2] <- 0
write.table(renov, "y3_middle/renov.txt", row.names = FALSE, col.names = FALSE)
rm(list = ls())

## ---------------------------------------------------------------------------------------------------------
## Elementary School
## ---------------------------------------------------------------------------------------------------------
## NOTE: re-coding hasn't been done.
library(foreign)
element <- read.spss("y3_elementary.sav", to.data.frame = TRUE)
element.data <- element[!is.na(element$Y3E_CLASS), ]

sid.element <- as.numeric(element.data$SCHID) - 100
nstudent <- table(sid.element)
school <- as.numeric(which(nstudent > 20))
nschool <- length(school)
element.newdata <- element.data[sid.element == school[1], ]
sid.newelement <- sid.element[sid.element == school[1]]
for (i in 2:nschool) {
  element.newdata <- rbind(element.newdata, element.data[sid.element == school[i], ])
  sid.newelement <- c(sid.newelement, sid.element[sid.element == school[i]])
}

mental <- cbind(
  as.numeric(element.newdata$Y3E_ST16_1) > 3, as.numeric(element.newdata$Y3E_ST16_2) > 3,
  as.numeric(element.newdata$Y3E_ST16_3) > 3, as.numeric(element.newdata$Y3E_ST16_4) > 3,
  as.numeric(element.newdata$Y3E_ST16_5) > 3, as.numeric(element.newdata$Y3E_ST16_6) > 3
) * 1
new.element <- mental # 1 - 6

citizen <- cbind(
  as.numeric((element.newdata$Y3E_ST17_1)) > 3, as.numeric((element.newdata$Y3E_ST17_2)) > 3,
  as.numeric((element.newdata$Y3E_ST17_3)) > 3, as.numeric((element.newdata$Y3E_ST17_4)) > 3,
  as.numeric((element.newdata$Y3E_ST17_5)) > 3, as.numeric((element.newdata$Y3E_ST17_7)) > 3,
  as.numeric((element.newdata$Y3E_ST17_8)) > 3, as.numeric((element.newdata$Y3E_ST17_9)) > 3,
  as.numeric((element.newdata$Y3E_ST17_10)) > 3, as.numeric((element.newdata$Y3E_ST17_11)) > 3,
  as.numeric((element.newdata$Y3E_ST17_12)) > 3, as.numeric((element.newdata$Y3E_ST17_13)) > 3,
  as.numeric((element.newdata$Y3E_ST17_14)) > 3, as.numeric((element.newdata$Y3E_ST17_15)) > 3,
  as.numeric((element.newdata$Y3E_ST17_16)) > 3, as.numeric((element.newdata$Y3E_ST17_17)) > 3
) * 1
new.element <- cbind(new.element, citizen) # 7 - 22

efficacy <- cbind(
  as.numeric((element.newdata$Y3E_ST18_1)) > 3, as.numeric((element.newdata$Y3E_ST18_2)) > 3,
  as.numeric((element.newdata$Y3E_ST18_3)) > 3, as.numeric((element.newdata$Y3E_ST18_4)) > 3,
  as.numeric((element.newdata$Y3E_ST18_5)) > 3, as.numeric((element.newdata$Y3E_ST18_8)) > 3,
  as.numeric((element.newdata$Y3E_ST18_9)) > 3, as.numeric((element.newdata$Y3E_ST18_10)) > 3
) * 1
new.element <- cbind(new.element, efficacy) # 23 - 30

belief <- cbind(
  as.numeric((element.newdata$Y3E_ST21_1)) > 3, as.numeric((element.newdata$Y3E_ST21_2)) > 3,
  as.numeric((element.newdata$Y3E_ST21_3)) > 3
) * 1
new.element <- cbind(new.element, belief) # 31 - 33

stress <- cbind(
  as.numeric((element.newdata$Y3E_ST24_1)) > 3, as.numeric((element.newdata$Y3E_ST24_2)) > 3,
  as.numeric((element.newdata$Y3E_ST24_3)) > 3, as.numeric((element.newdata$Y3E_ST24_4)) > 3,
  as.numeric((element.newdata$Y3E_ST24_5)) > 3, as.numeric((element.newdata$Y3E_ST24_6)) > 3,
  as.numeric((element.newdata$Y3E_ST24_7)) > 3
) * 1
new.element <- cbind(new.element, stress) # 34 - 40

friends <- cbind(
  as.numeric((element.newdata$Y3E_ST36_1)) > 3, as.numeric((element.newdata$Y3E_ST36_2)) > 3,
  as.numeric((element.newdata$Y3E_ST36_3)) > 3, as.numeric((element.newdata$Y3E_ST36_4)) > 3,
  as.numeric((element.newdata$Y3E_ST36_5)) > 3, as.numeric((element.newdata$Y3E_ST36_6)) > 3
) * 1
new.element <- cbind(new.element, friends) # 41 - 46

esteem <- cbind(
  as.numeric((element.newdata$Y3E_ST19_1)) > 3, as.numeric((element.newdata$Y3E_ST19_2)) > 3,
  as.numeric((element.newdata$Y3E_ST19_3)) > 3, as.numeric((element.newdata$Y3E_ST19_7)) > 3
) * 1
new.element <- cbind(new.element, esteem) # 47 - 50

academic <- cbind(
  as.numeric((element.newdata$Y3E_ST23_1)) > 3, as.numeric((element.newdata$Y3E_ST23_6)) > 3,
  as.numeric((element.newdata$Y3E_ST23_7)) > 3, as.numeric((element.newdata$Y3E_ST23_8)) > 3,
  as.numeric((element.newdata$Y3E_ST23_9)) > 3, as.numeric((element.newdata$Y3E_ST23_10)) > 3,
  as.numeric((element.newdata$Y3E_ST23_11)) > 3, as.numeric((element.newdata$Y3E_ST23_12)) > 3,
  as.numeric((element.newdata$Y3E_ST23_13)) > 3, as.numeric((element.newdata$Y3E_ST23_14)) > 3
) * 1
new.element <- cbind(new.element, academic) # 51 - 60

appear <- (as.numeric((element.newdata$Y3E_ST18_6)) > 3) * 1 + (as.numeric((element.newdata$Y3E_ST18_7)) > 3) * 1
appear <- (appear > 0) * 1

positive <- (as.numeric((element.newdata$Y3E_ST19_4)) > 3) * 1 + (as.numeric((element.newdata$Y3E_ST19_5)) > 3) * 1
positive <- (positive > 0) * 1
new.element <- cbind(new.element, appear, positive) # 61 - 62

new.element[is.na(new.element)] <- 0

for (i in 1:nschool) {
  if (i < 10) {
    fopen <- paste("y3_elementary/item0", i, ".txt", sep = "")
  } else {
    fopen <- paste("y3_elementary/item", i, ".txt", sep = "")
  }

  temp <- new.element[sid.newelement == school[i], ]

  write.table(temp, fopen, row.names = FALSE, col.names = FALSE)
}
selement <- read.spss("y3_school_elementary.sav", to.data.frame = TRUE)
renov <- as.numeric(selement$Y3E_SCH1_1)
renov[renov == 2] <- 0
renov <- renov[school]
table(renov)
write.table(renov, "y3_elementary/renov.txt", row.names = FALSE, col.names = FALSE)

rm(list = ls())
