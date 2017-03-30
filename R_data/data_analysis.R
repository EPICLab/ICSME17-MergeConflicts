library(dplyr)
library(cowplot)
library(ggrepel)
library(extrafont)
library(reshape2)
#source("effectiveness.R")  # BROKEN FOR NOW

if (!exists("survey")) {
  print("Reading csv data into 'survey' dataframe...")
  survey <- read.csv("data.csv", 
                     header=TRUE, sep=",", na.strings=c("", "NA"))
}

# CORRELATION ANALYSIS
# Merge Conflict Difficulty Factors
time_to_resolve       <- as.numeric(survey$Q50_4[which(!is.na(as.numeric(survey$Q50_4)))])-35
number_lines          <- as.numeric(survey$Q50_5[which(!is.na(as.numeric(survey$Q50_5)))])-35
complexity_lines      <- as.numeric(survey$Q50_6[which(!is.na(as.numeric(survey$Q50_6)))])-35
number_files          <- as.numeric(survey$Q50_7[which(!is.na(as.numeric(survey$Q50_7)))])-35
complexity_files      <- as.numeric(survey$Q50_8[which(!is.na(as.numeric(survey$Q50_8)))])-35
nonfunctional_changes <- as.numeric(survey$Q50_9[which(!is.na(as.numeric(survey$Q50_9)))])-35
dependencies          <- as.numeric(survey$Q50_10[which(!is.na(as.numeric(survey$Q50_10)))])-35
atomicity             <- as.numeric(survey$Q50_11[which(!is.na(as.numeric(survey$Q50_11)))])-35
local_expertise_mc    <- as.numeric(survey$Q50_16[which(!is.na(as.numeric(survey$Q50_16)))])-35

# Conflict Resolution Difficulty Factors
amount_info           <- as.numeric(survey$Q36_1[which(!is.na(as.numeric(survey$Q36_1)))])-12
understandability     <- as.numeric(survey$Q36_2[which(!is.na(as.numeric(survey$Q36_2)))])-12
tool_for_history      <- as.numeric(survey$Q36_3[which(!is.na(as.numeric(survey$Q36_3)))])-12
project_complexity    <- as.numeric(survey$Q36_4[which(!is.na(as.numeric(survey$Q36_4)))])-12
info_presentation     <- as.numeric(survey$Q36_5[which(!is.na(as.numeric(survey$Q36_5)))])-12
tool_trust            <- as.numeric(survey$Q36_6[which(!is.na(as.numeric(survey$Q36_6)))])-12
informative_commits   <- as.numeric(survey$Q36_7[which(!is.na(as.numeric(survey$Q36_7)))])-12
changing_assumptions  <- as.numeric(survey$Q36_8[which(!is.na(as.numeric(survey$Q36_8)))])-12
project_culture       <- as.numeric(survey$Q36_10[which(!is.na(as.numeric(survey$Q36_10)))])-12
local_expertise_mcr   <- as.numeric(survey$Q36_11[which(!is.na(as.numeric(survey$Q36_11)))])-12

cor.test(lines, complexity, test='pearson')
cor.test(lines, local_expertise, test='pearson')

# Wilcoxon rank sum test to determine statistical order of factors
dd = data.frame(time_to_resolve, number_lines)

# boxplot chart for means on the 'Complexity of conflicting lines of code' question
mydata <- data.frame(exp_labels = c("1 developer", "2-5 developers", "6-10 developers", "11-50 developers", "51+ developers"),
                     means = c(3.12, 3.38, 3.47, 4.0, 3.69))

bplot <- barplot(mydata$means-1, main="", axes=F, ylim=c(0,4), 
                 names.arg=mydata$exp_labels, cex.names=1.1)
axis(side = 2, at=(1:5)-1, labels=(1:5))
abline(h=0)
text(x = bplot, y = mydata$means-1, label = round(mydata$means, 2), pos = 3, cex = 1.2)
text(x = -0.38, y = 2.0, labels = "Likert Score", srt = 90, xpd=T, cex = 1.2)


# GENDER
gender.male        <- survey[grep("Male", survey$Gender), ]
gender.female      <- survey[grep("Female", survey$Gender), ]

# ROLES
roles.developers  <- survey[grep("Software Engineer/Developer", survey$Roles), ]
roles.engineers   <- survey[grep("Systems Engineer", survey$Roles), ]
roles.architects  <- survey[grep("System Architect", survey$Roles), ]
roles.sysadmins   <- survey[grep("Systems Administrator", survey$Roles), ]
roles.devops      <- survey[grep("DevOps", survey$Roles), ]
roles.maintainers <- survey[grep("Project Maintainer", survey$Roles), ]
roles.managers    <- survey[grep("Project Manager", survey$Roles), ]
roles.other       <- survey[grep("Other", survey$Roles), ]

# EXPERIENCE
exp.1_5           <- survey[grep("1-5", survey$Experience), ]
exp.6_10          <- survey[grep("6-10", survey$Experience), ]
exp.11_15         <- survey[grep("11-15", survey$Experience), ]
exp.16_20         <- survey[grep("16-20", survey$Experience), ]
exp.21_25         <- survey[grep("21-25", survey$Experience), ]
exp.26_u          <- survey[grep("25", survey$Experience), ]

# SOURCE DISTRIBUTION MODEL
source.open       <- survey[grep("Open-Source Projects", survey$DevType), ]
source.closed     <- survey[grep("Closed-Source Projects", survey$DevType), ]
source.split      <- survey[grep("I split my time evenly", survey$DevType), ]

# PROJECT SIZE
proj_size.1       <- survey[grep("1 developer", survey$TeamSize), ]
proj_size.2_5     <- survey[grep("2-5 developers", survey$TeamSize), ]
proj_size.6_10    <- survey[grep("6-10 developers", survey$TeamSize), ]
proj_size.11_50   <- survey[grep("11-50 developers", survey$TeamSize), ]
proj_size.50_u    <- survey[grep("51", survey$TeamSize), ]


opened_devs <- survey[survey$Q31 == 1,]
opened_devs <- opened_devs$Q36_3
opened_devs <- opened_devs[!is.na(opened_devs)]
# Q36_3 is for Tool support for examining development history
closed_devs <- survey[survey$Q31 == 2,]
closed_devs <- closed_devs$Q36_3
closed_devs <- closed_devs[!is.na(closed_devs)]
wilcox.test(opened_devs, closed_devs)

answer_people <- survey[which(!is.na(survey$Q50_16)),]
occurences <- survey$Q39
occurences <- occurences[!is.na(occurences)]

# prop.test(table(survey$Experience, survey$Q36_1), correct=FALSE)
# 
# #Experience group analysis
# #Gender difference
# #prop.test()
# 
# 
# # generate plots for each combination of merge conflict size/complexity
# 
# 
# subdata <- survey[!(is.na(survey$Q19_1) | is.na(survey$Q19_2) | is.na(survey$Q19_3) | is.na(survey$Q19_4)), ]
# subdata <- data.frame(survey$Q19_1, survey$Q19_2, survey$Q19_3, survey$Q19_4)
# names(subdata) <- c("simple_small", "simple_large", "complex_small", "complex_large")
# # Pearson's correlation test
# cor.ss_sl <- cor.test(as.vector(subdata$simple_small), as.vector(subdata$simple_large), method="pearson", alternative="two.sided")
# # Wilcoxon signed rank test with continuity correction
# wil.ss_sl <- wilcox.test(subdata$simple_small, subdata$simple_large, paired = TRUE)