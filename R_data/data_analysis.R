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

# Q19: How effectively does your current toolset support each of the following scenarios?
toolEffect <- list(EffectiveTags = c("Extremely Effective", "Very effective", "Moderately effective"), 
                    IneffectiveTags = c("Slightly effective", "Not effective at all"))

# complexity-based evaluation
toolEffect$SimpleEffective    <- sum(is.element(survey$Q19_1, tool_effect$Effective_Tags)) + sum(is.element(survey$Q19_2, tool_effect$Effective_Tags))
toolEffect$SimpleIneffective  <- sum(is.element(survey$Q19_1, tool_effect$Ineffective_Tags)) + sum(is.element(survey$Q19_2, tool_effect$Ineffective_Tags))
toolEffect$ComplexEffective   <- sum(is.element(survey$Q19_3, tool_effect$Effective_Tags)) + sum(is.element(survey$Q19_4, tool_effect$Effective_Tags))
toolEffect$ComplexIneffective <- sum(is.element(survey$Q19_3, tool_effect$Ineffective_Tags)) + sum(is.element(survey$Q19_4, tool_effect$Ineffective_Tags))

# size-based evaluation
toolEffect$SmallEffective     <- sum(is.element(survey$Q19_1, tool_effect$Effective_Tags)) + sum(is.element(survey$Q19_3, tool_effect$Effective_Tags))
toolEffect$SmallIneffective   <- sum(is.element(survey$Q19_1, tool_effect$Ineffective_Tags)) + sum(is.element(survey$Q19_3, tool_effect$Ineffective_Tags))
toolEffect$LargeEffective     <- sum(is.element(survey$Q19_2, tool_effect$Effective_Tags)) + sum(is.element(survey$Q19_4, tool_effect$Effective_Tags))
toolEffect$LargeIneffective   <- sum(is.element(survey$Q19_2, tool_effect$Ineffective_Tags)) + sum(is.element(survey$Q19_4, tool_effect$Ineffective_Tags))

toolEffect$ComplexityTable <- matrix(c())

# filter on specific roles
developers  <- survey[grep("Software Engineer/Developer", survey$Roles), ]
engineers   <- survey[grep("Systems Engineer", survey$Roles), ]
architects  <- survey[grep("System Architect", survey$Roles), ]
sysadmins   <- survey[grep("Systems Administrator", survey$Roles), ]
devops      <- survey[grep("DevOps", survey$Roles), ]
maintainers <- survey[grep("Project Maintainer", survey$Roles), ]
managers    <- survey[grep("Project Manager", survey$Roles), ]
other       <- survey[grep("Other", survey$Roles), ]

experienced     <- survey[grep("26", survey$Experience), ]
mid_experience  <- survey[grep("16", survey$Experience), ]
exp_maintainers <- merge(experienced, maintainers)
dev_sysEng      <- merge(developers, engineers)
females         <- survey[grep("Female", survey$Gender), ]

# determine combinations of roles
#devops_developers <- merge(devops, developers)

# generate plots for each combination of merge conflict size/complexity


subdata <- survey[!(is.na(survey$Q19_1) | is.na(survey$Q19_2) | is.na(survey$Q19_3) | is.na(survey$Q19_4)), ]
subdata <- data.frame(survey$Q19_1, survey$Q19_2, survey$Q19_3, survey$Q19_4)
names(subdata) <- c("simple_small", "simple_large", "complex_small", "complex_large")
# Pearson's correlation test
cor.ss_sl <- cor.test(as.vector(subdata$simple_small), as.vector(subdata$simple_large), method="pearson", alternative="two.sided")
# Wilcoxon signed rank test with continuity correction
wil.ss_sl <- wilcox.test(subdata$simple_small, subdata$simple_large, paired = TRUE)