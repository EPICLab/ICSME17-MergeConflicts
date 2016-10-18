library(dplyr)
library(cowplot)
library(ggrepel)
library(extrafont)
#source("effectiveness.R")  # BROKEN FOR NOW

if (!exists("survey")) {
  print("Reading csv data into 'survey' dataframe...")
  survey <- read.csv("/home/nelsonni/Documents/Research/Merge Conflicts Project/SANERPaper/R_data/survey.csv", 
                     header=TRUE, sep=",", na.strings=c("", "NA"))
}

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