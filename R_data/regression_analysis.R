survey2 <- read.csv("/home/nelsonni/Documents/Research/Merge Conflicts Project/R data/survey2.csv", header=TRUE, sep=",", na.strings=c("", "NA"))
sysadmins   <- survey2[grep("Systems Administrator", survey2$Roles), ]

library(MASS)
y=lm(Complex_Large ~ Experience + as.factor(DevType) + TeamSize + 
                     ConflictFreq + as.factor(Gender), data = survey2)
astepAIC <- stepAIC(y, direction=c("both"))
summary(astepAIC)


simple_eff <- length(survey$Q19_1[which(survey$Q19_1=="Extremely Effective")]) + 
  length(survey$Q19_2[which(survey$Q19_2=="Extremely Effective")])

complex_eff <- length(survey$Q19_3[which(survey$Q19_3=="Extremely Effective") ]) + 
  length(survey$Q19_4[which(survey$Q19_4=="Extremely Effective") ])

simple_noneff <- length(survey$Q19_1[which(survey$Q19_1=="Not effective at all")]) + 
  length(survey$Q19_2[which(survey$Q19_2=="Not effective at all")])

complex_noneff <- length(survey$Q19_3[which(survey$Q19_3=="Not effective at all") ]) + 
             length(survey$Q19_4[which(survey$Q19_4=="Not effective at all") ])

Effects <- matrix(c(simple_eff, complex_eff, simple_noneff, simple_eff), 
                  nrow = 2,
                  dimnames = list(c("Simple", "Complex"),
                                  c("Extremely Effective", "Not effective at all")))
