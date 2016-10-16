library(ggplot2)
# QUESTION 50 (v1): Factors that make merge conflict resolutions difficult.

plotExtremes <- function(group, title) {
  percents <- c((group[1] + group[2] + group[3]/2)/sum(group)*100, (group[3]/2 + group[4] + group[5])/sum(group)*100)
  bplot <- barplot(percents, main=title, ylim=c(0,100), names.arg=c("No Effect", "Effect"))
  text(x = bplot, y = percents, label = percents, pos = 3, cex = 0.8)
}

plotGender <- function(target) {
  plotExtremes(target$everybody, "Everybody")
  plotExtremes(target$male, "Male")
  plotExtremes(target$female, "Female")
}

plotSourceModels <- function(target) {
  plotExtremes(target$everybody, "Everybody")
  plotExtremes(target$open, "Open")
  plotExtremes(target$closed, "Closed")
  plotExtremes(target$both, "Both")
}

plotRoles <- function(target) {
  plotExtremes(target$everybody, "Everybody")
  plotExtremes(target$soft_eng, "Software Engineers")
  plotExtremes(target$sys_eng, "System Engineers")
  plotExtremes(target$sys_arc, "System Architects")
  plotExtremes(target$sys_admin, "Systems Administrators")
  plotExtremes(target$devops, "DevOps")
  plotExtremes(target$proj_maint, "Project Maintainers")
  plotExtremes(target$proj_manag, "Project Managers")  
}

code_info <- list()
code_info$everybody  <- c(2, 21, 38, 48, 32)
# GENDER
code_info$male       <- c(2, 19, 35, 42, 31)
code_info$female     <- c(0, 2, 0, 4, 1)
# SOURCE DISTRIBUTION MODEL
code_info$open       <- c(1, 1, 8, 7, 3)
code_info$closed     <- c(0, 15, 23, 33, 19)
code_info$both       <- c(1, 5, 7, 8, 10)
# ROLES
code_info$soft_eng   <- c(2, 18, 36, 47, 31)
code_info$sys_eng    <- c(0, 3, 9, 11, 6)
code_info$sys_arc    <- c(0, 4, 13, 19, 11)
code_info$sys_admin  <- c(0, 3, 10, 8, 0)
code_info$devops     <- c(0, 4, 10, 18, 15)
code_info$proj_maint <- c(0, 4, 7, 12, 10)
code_info$proj_manag <- c(1, 5, 11, 11, 10)

code_expertise <- list()
code_expertise$everybody  <- c(1, 17, 38, 49, 36)
# GENDER
code_expertise$male       <- c(1, 15, 33, 46, 34)
code_expertise$female     <- c(0, 1, 4, 2, 0)
# SOURCE DISTRIBUTION MODEL
code_expertise$open       <- c(0, 0, 8, 8, 4)
code_expertise$closed     <- c(0, 13, 22, 29, 26)
code_expertise$both       <- c(1, 4, 8, 12, 6)
# ROLES
code_expertise$soft_eng   <- c(1, 15, 35, 47, 36)
code_expertise$sys_eng    <- c(0, 2, 9, 9, 9)
code_expertise$sys_arc    <- c(0, 4, 9, 18, 16)
code_expertise$sys_admin  <- c(0, 3, 7, 7, 4)
code_expertise$devops     <- c(0, 4, 9, 19, 15)
code_expertise$proj_maint <- c(0, 2, 12, 8, 11)
code_expertise$proj_manag <- c(0, 5, 12, 10, 11)

ease_understand <- list()
ease_understand$everybody  <- c(0, 14, 25, 65, 37)
# GENDER
ease_understand$male       <- c(0, 11, 23, 63, 32)
ease_understand$female     <- c(0, 2, 1, 2, 2)
# SOURCE DISTRIBUTION MODEL
ease_understand$open       <- c(0, 1, 1, 10, 8)
ease_understand$closed     <- c(0, 10, 16, 43, 21)
ease_understand$both       <- c(0, 3, 8, 12, 8)
# ROLES
ease_understand$soft_eng   <- c(0, 13, 23, 64, 34)
ease_understand$sys_eng    <- c(0, 3, 2, 13, 11)
ease_understand$sys_arc    <- c(0, 1, 4, 24, 18)
ease_understand$sys_admin  <- c(0, 1, 2, 11, 7)
ease_understand$devops     <- c(0, 3, 7, 20, 17)
ease_understand$proj_maint <- c(0, 3, 4, 19, 7)
ease_understand$proj_manag <- c(0, 4, 5, 14, 15)

info_presentation <- list()
info_presentation$everybody  <- c(4, 24, 47, 32, 34)
# GENDER
info_presentation$male       <- c(3, 22, 43, 29, 32)
info_presentation$female     <- c(1, 2, 1, 2, 1)
# SOURCE DISTRIBUTION MODEL
info_presentation$open       <- c(0, 5, 5, 5, 5)
info_presentation$closed     <- c(3, 16, 28, 21, 22)
info_presentation$both       <- c(1, 3, 14, 6, 7)
# ROLES
info_presentation$soft_eng   <- c(4, 23, 45, 29, 33)
info_presentation$sys_eng    <- c(0, 8, 7, 9, 5)
info_presentation$sys_arc    <- c(0, 7, 11, 12, 17)
info_presentation$sys_admin  <- c(0, 3, 7, 8, 3)
info_presentation$devops     <- c(0, 9, 13, 14, 11)
info_presentation$proj_maint <- c(1, 7, 8, 11, 6)
info_presentation$proj_manag <- c(1, 10, 8, 8, 11)

trustworthiness <- list()
trustworthiness$everybody  <- c(17, 29, 39, 32, 24)
# GENDER
trustworthiness$male       <- c(14, 26, 35, 31, 23)
trustworthiness$female     <- c(3, 0, 3, 0, 1)
# SOURCE DISTRIBUTION MODEL
trustworthiness$open       <- c(1, 6, 5, 3, 5)
trustworthiness$closed     <- c(14, 14, 25, 24, 13)
trustworthiness$both       <- c(2, 9, 9, 5, 6)
# ROLES
trustworthiness$soft_eng   <- c(17, 28, 37, 31, 21)
trustworthiness$sys_eng    <- c(5, 9, 6, 3, 6)
trustworthiness$sys_arc    <- c(2, 13, 12, 10, 10)
trustworthiness$sys_admin  <- c(1, 4, 8, 4, 4)
trustworthiness$devops     <- c(7, 10, 11, 12, 7)
trustworthiness$proj_maint <- c(7, 7, 8, 7, 4)
trustworthiness$proj_manag <- c(4, 11, 5, 8, 10)

tools_history <- list()
tools_history$everybody  <- c(16, 40, 31, 32, 22)
# GENDER
tools_history$male       <- c(14, 35, 28, 30, 22)
tools_history$female     <- c(2, 1, 3, 1, 0)
# SOURCE DISTRIBUTION MODEL
tools_history$open       <- c(0, 5, 4, 5, 6)
tools_history$closed     <- c(13, 28, 18, 21, 10)
tools_history$both       <- c(3, 7, 9, 6, 6)
# ROLES
tools_history$soft_eng   <- c(16, 39, 28, 31, 20)
tools_history$sys_eng    <- c(3, 10, 6, 6, 4)
tools_history$sys_arc    <- c(4, 9, 12, 11, 11)
tools_history$sys_admin  <- c(2, 7, 5, 5, 2)
tools_history$devops     <- c(4, 14, 10, 10, 9)
tools_history$proj_maint <- c(3, 7, 10, 6, 7)
tools_history$proj_manag <- c(4, 10, 8, 7, 9)

proj_complexity <- list()
proj_complexity$everybody  <- c(6, 38, 39, 41, 17)
# GENDER
proj_complexity$male       <- c(6, 34, 36, 36, 17)
proj_complexity$female     <- c(0, 1, 2, 4, 0)
# SOURCE DISTRIBUTION MODEL
proj_complexity$open       <- c(1, 4, 7, 7, 1)
proj_complexity$closed     <- c(3, 28, 23, 24, 12)
proj_complexity$both       <- c(2, 6, 9, 10, 4)
# ROLES
proj_complexity$soft_eng   <- c(5, 37, 37, 39, 16)
proj_complexity$sys_eng    <- c(2, 8, 6, 10, 3)
proj_complexity$sys_arc    <- c(2, 8, 9, 17, 11)
proj_complexity$sys_admin  <- c(1, 2, 6, 7, 5)
proj_complexity$devops     <- c(3, 10, 11, 14, 9)
proj_complexity$proj_maint <- c(3, 7, 7, 11, 5)
proj_complexity$proj_manag <- c(2, 10, 9, 11, 6)

commit_messages <- list()
commit_messages$everybody  <- c(18, 32, 30, 44, 17)
# GENDER
commit_messages$male       <- c(17, 30, 27, 40, 15)
commit_messages$female     <- c(1, 0, 1, 4, 1)
# SOURCE DISTRIBUTION MODEL
commit_messages$open       <- c(2, 4, 5, 7, 2)
commit_messages$closed     <- c(13, 22, 19, 26, 10)
commit_messages$both       <- c(3, 6, 6, 11, 5)
# ROLES
commit_messages$soft_eng   <- c(18, 30, 29, 41, 16)
commit_messages$sys_eng    <- c(4, 5, 7, 12, 1)
commit_messages$sys_arc    <- c(3, 5, 15, 16, 8)
commit_messages$sys_admin  <- c(0, 5, 6, 7, 3)
commit_messages$devops     <- c(6, 8, 11, 15, 7)
commit_messages$proj_maint <- c(2, 5, 8, 13, 5)
commit_messages$proj_manag <- c(4, 4, 13, 12, 5)

changing_assumptions <- list()
changing_assumptions$everybody  <- c(8, 27, 45, 36, 25)
# GENDER
changing_assumptions$male       <- c(8, 24, 39, 35, 23)
changing_assumptions$female     <- c(0, 2, 4, 1, 0)
# SOURCE DISTRIBUTION MODEL
changing_assumptions$open       <- c(2, 3, 7, 5, 3)
changing_assumptions$closed     <- c(3, 21, 27, 22, 17)
changing_assumptions$both       <- c(3, 3, 11, 9, 5)
# ROLES
changing_assumptions$soft_eng   <- c(8, 26, 42, 36, 22)
changing_assumptions$sys_eng    <- c(2, 5, 6, 10, 6)
changing_assumptions$sys_arc    <- c(2, 5, 15, 15, 10)
changing_assumptions$sys_admin  <- c(1, 4, 6, 6, 4)
changing_assumptions$devops     <- c(4, 5, 14, 14, 10)
changing_assumptions$proj_maint <- c(4, 5, 10, 8, 6)
changing_assumptions$proj_manag <- c(4, 8, 12, 9, 5)

proj_culture <- list()
proj_culture$everybody  <- c(13, 37, 43, 27, 21)
# GENDER
proj_culture$male       <- c(11, 35, 40, 25, 18)
proj_culture$female     <- c(1, 1, 3, 1, 1)
# SOURCE DISTRIBUTION MODEL
proj_culture$open       <- c(3, 4, 7, 3, 3)
proj_culture$closed     <- c(8, 25, 23, 19, 15)
proj_culture$both       <- c(2, 8, 13, 5, 3)
# ROLES
proj_culture$soft_eng   <- c(13, 35, 39, 26, 21)
proj_culture$sys_eng    <- c(5, 6, 6, 9, 3)
proj_culture$sys_arc    <- c(5, 8, 11, 13, 10)
proj_culture$sys_admin  <- c(1, 6, 3, 6, 5)
proj_culture$devops     <- c(8, 10, 7, 12, 10)
proj_culture$proj_maint <- c(7, 8, 7, 8, 3)
proj_culture$proj_manag <- c(7, 9, 8, 9, 5)