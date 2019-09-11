library(dplyr)
eva_33 <- read_csv("~/Desktop/eva_33.csv")
eva_33 = as.data.frame(cbind(eva_33$ID_encrypt, scale(eva_33[,2:15], center = T)))
names(eva_33)[1] = "ID_encrypt"

mean_score_teachers <- eva_33 %>% select(My_general_evaluation_of_this_teacher_is_1, My_general_evaluation_of_this_teacher_is_2_, This_teacher_is_organized_and_prepared_1, This_teacher_is_organized_and_prepared_2_, This_teacher_gives_interesting_activities_1, This_teacher_explains_things_clearly_1, This_teacher_answers_questions_well_1, This_teacher_gives_students_chances_to_participate_in_class_1_, This_teacher_gives_interesting_activities_2_, This_teacher_explains_things_clearly_2_, This_teacher_answers_questions_well_2_, This_teacher_gives_students_chances_to_participate_in_class_2_) %>% 
  rowMeans(., na.rm = T)

mean_score_courses <- eva_33 %>% select(My_general_evaluation_of_this_course_is_1, My_general_evaluation_of_this_course_is_2_) %>% rowMeans(., na.rm = T)

evaluation_33 <- data.frame(eva_33$ID_encrypt, mean_score_courses, mean_score_teachers)

names(evaluation_33)[1] = "ID_encrypt"

write_csv(evaluation_33, "evaluation_33.csv")



######34
eva_34 <- read_csv("~/Desktop/eva_34.csv")
eva_34 <- as.data.frame(eva_34)
eva_34 = as.data.frame(cbind(eva_34$ID_encrypt, scale(eva_34[,2:15], center = T)))
names(eva_34)[1] = "ID_encrypt"


mean_score_teachers <- eva_34 %>% select(My_general_evaluation_of_this_teacher_is_1_, My_general_evaluation_of_this_teacher_is_2_, This_teacher_is_organized_and_prepared_1_, This_teacher_is_organized_and_prepared_2_, This_teacher_gives_interesting_activities_1_, This_teacher_explains_things_clearly_1_, This_teacher_answers_questions_well_1_, This_teacher_gives_students_chances_to_participate_in_class_1_, This_teacher_gives_interesting_activities_2_, This_teacher_explains_things_clearly_2_, This_teacher_answers_questions_well_2_, This_teacher_gives_students_chances_to_participate_in_class_2_) %>% 
  rowMeans(., na.rm = T)

mean_score_courses <- eva_34 %>% select(My_general_evaluation_of_this_course_is_1_, My_general_evaluation_of_this_course_is_2_) %>% rowMeans(., na.rm = T)

evaluation_34 <- data.frame(eva_34$ID_encrypt, mean_score_courses, mean_score_teachers)

write_csv(evaluation_34, "evaluation_34.csv")
#####46

eva_46 <- read_csv("~/Desktop/eva_46.csv")
eva_46 = as.data.frame(eva_46)

eva_46 <- as.data.frame(eva_46)
for(i in 2:ncol(eva_46)){
  eva_46[,i] = as.numeric(eva_46[,i])
}

eva_46 = as.data.frame(cbind(eva_46$ID_encrypt, scale(eva_46[,2:15], center = T)))
names(eva_46)[1] = "ID_encrypt"



mean_score_teachers <- eva_46 %>% select(My_general_evaluation_of_this_teacher_is_1_, My_general_evaluation_of_this_teacher_is_2_, This_teacher_is_organized_and_prepared_1_, This_teacher_is_organized_and_prepared_2_, This_teacher_gives_interesting_activities_1_, This_teacher_explains_things_clearly_1_, This_teacher_answers_questions_well_1_, This_teacher_gives_students_chances_to_participate_in_class_1_, This_teacher_gives_interesting_activities_2_, This_teacher_explains_things_clearly_2_, This_teacher_answers_questions_well_2_, This_teacher_gives_students_chances_to_participate_in_class_2_) %>% 
  rowMeans(., na.rm = T)

mean_score_courses <- eva_46 %>% select(My_general_evaluation_of_this_course_is_1_, My_general_evaluation_of_this_course_is_2_) %>% rowMeans(., na.rm = T)

evaluation_46 <- data.frame(eva_46$ID_encrypt, mean_score_courses, mean_score_teachers)

write_csv(evaluation_46, "evaluation_46.csv")
######
eva_80 <- read_csv("~/Desktop/eva_80.csv")
eva_80 = as.data.frame(eva_80)


for(i in 2:ncol(eva_80)){
  eva_80[,i] = as.numeric(eva_80[,i])
}


eva_80 = as.data.frame(cbind(eva_80$ID_encrypt, scale(eva_80[,2:38], center = T)))
names(eva_80)[1] = "ID_encrypt"

names(eva_80)
mean_score_teachers <- eva_80 %>% select(2,4,5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29) %>% rowMeans(., na.rm = T)

mean_score_courses <- eva_80 %>% select(My_general_evaluation_of_my_11_00_12_00_course_is__24_, My_general_evaluation_of_my_9_00_11_00_am_course_is__12_, My_general_evaluation_of_this_course_is__36_, My_general_evaluation_of_this_course_is__48_) %>% rowMeans(., na.rm = T)

mean_score_on_personal_improvement <- eva_80 %>% select(31,32,33,34,35) %>% rowMeans(., na.rm = T)

mean_score_on_program <- eva_80 %>% select(satisfied_with_the_AIEP_program_in_general___58_,satisfied_with_California_Tours___73_) %>% rowMeans(., na.rm = T)

evaluation_80 <- data.frame(eva_80$ID_encrypt, mean_score_courses, mean_score_on_personal_improvement, mean_score_on_program, mean_score_teachers, eva_80$Will_you_recommend_ALC_to_your_friends___79_)

write.csv(evaluation_80, "evaluation_80.csv")


###########81

write_csv(eva_81, "eva_81.csv")

read_csv("eva_81.csv")

eva_81 <- Program_Evaluations_81_export[Program_Evaluations_81_export$ID_encrypt !="NULL",]

eva_81 = eva_81[eva_81$ID_encrypt != "NULL", ]

eva_81 = eva_81[-c(593),]

eva_81 <- eva_81[,-c(9,33)]
eva_81 <- eva_81[,-c(16,24)]


for(i in 2:ncol(eva_81)){
  eva_81[,i] = as.numeric(eva_81[,i])
}


eva_81 = as.data.frame(cbind(eva_81$ID_encrypt, scale(eva_81[,2:38], center = T)))
names(eva_81)[1] = "ID_encrypt"

names(eva_81)
mean_score_teachers <- eva_81 %>% select(2,4,5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29) %>% rowMeans(., na.rm = T)

mean_score_courses <- eva_81 %>% select(My_general_evaluation_of_my_11_00_12_00_course_is__24_, My_general_evaluation_of_my_9_00_11_00_am_course_is__12_, My_general_evaluation_of_this_course_is__36_, My_general_evaluation_of_this_course_is__48_) %>% rowMeans(., na.rm = T)

mean_score_on_personal_improvement <- eva_81 %>% select(31,32,33,34,35) %>% rowMeans(., na.rm = T)

mean_score_on_program <- eva_81 %>% select(satisfied_with_the_AIEP_program_in_general___58_,satisfied_with_California_Tours___73_) %>% rowMeans(., na.rm = T)

evaluation_81 <- data.frame(eva_81$ID_encrypt, mean_score_courses, mean_score_on_personal_improvement, mean_score_on_program, mean_score_teachers, eva_81$Will_you_recommend_ALC_to_your_friends___79_)

which(evaluation_81$eva_81.Will_you_recommend_ALC_to_your_friends___79_ == "C- good")

which(evaluation_81$eva_81.Will_you_recommend_ALC_to_your_friends___79_ == "Hillside Residence Hall")

evaluation_81 = evaluation_81[-c(310,362),]

evaluation_81$eva_81.Will_you_recommend_ALC_to_your_friends___79_

levels(evaluation_81[,6])[levels(evaluation_81[,6]) == "NULL"] <- NA
levels(evaluation_81[,6])[levels(evaluation_81[,6]) == "Yes"] <- 1
levels(evaluation_81[,6])[levels(evaluation_81[,6]) == "No"] <- 0


write.csv(evaluation_81, "evaluation_81.csv")


#######82

write_csv(eva_82, "eva_82.csv")

read_csv("eva_82.csv")

eva_82 = eva_82[eva_82$ID_encrypt != "NULL", ]

eva_82 = as.data.frame(eva_82)

eva_82 <- eva_82[,-c(9,33)]
eva_82 <- eva_82[,-c(16,24)]

for(i in 2:ncol(eva_82)){
  eva_82[,i] = as.numeric(eva_82[,i])
}

eva_82 = as.data.frame(cbind(eva_82$ID_encrypt, scale(eva_82[,2:38], center = T)))
names(eva_82)[1] = "ID_encrypt"


mean_score_teachers <- eva_82 %>% select(2,4,5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29) %>% rowMeans(., na.rm = T)

mean_score_courses <- eva_82 %>% select(My_general_evaluation_of_my_11_00_12_00_course_is_24_, My_general_evaluation_of_my_9_00_11_00_am_course_is_12_, My_general_evaluation_of_this_course_is_49_) %>% rowMeans(., na.rm = T)

mean_score_on_personal_improvement <- eva_82 %>% select(31,32,33,34,35) %>% rowMeans(., na.rm = T)

mean_score_on_program <- eva_82 %>% select(How_satisfied_are_you_with_the_AIEP_program_in_general_59_,How_satisfied_are_you_with_USA_Student_Tours_74_) %>% rowMeans(., na.rm = T)

evaluation_82 <- data.frame(eva_82$ID_encrypt, mean_score_courses, mean_score_on_personal_improvement, mean_score_on_program, mean_score_teachers, eva_82$Will_you_recommend_ALC_to_your_friends_80_)

write.csv(evaluation_82, "evaluation_82.csv")


#############92

write_csv(eva_92, "eva_92.csv")

read_csv("eva_92.csv")

eva_92 = eva_92[eva_92$ID_encrypt != "NULL", ]

eva_92 = as.data.frame(eva_92)

eva_92 <- eva_92[,-c(9,33)]
eva_92 <- eva_92[,-c(16,24)]

for(i in 2:ncol(eva_92)){
  eva_92[,i] = as.numeric(eva_92[,i])
}

eva_92 = as.data.frame(cbind(eva_92$ID_encrypt, scale(eva_92[,2:38], center = T)))
names(eva_92)[1] = "ID_encrypt"

mean_score_teachers <- eva_92 %>% select(2,4,5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29) %>% rowMeans(., na.rm = T)

mean_score_courses <- eva_92 %>% select(My_general_evaluation_of_my_9_00_11_00_am_course_is_1_, My_general_evaluation_of_my_11_00_12_00_course_is_2_, My_general_evaluation_of_this_course_is_3_, My_general_evaluation_of_this_course_is_4_) %>% rowMeans(., na.rm = T)

mean_score_on_personal_improvement <- eva_92 %>% select(31,32,33,34,35) %>% rowMeans(., na.rm = T)

mean_score_on_program <- eva_92 %>% select(`_How_satisfied_are_you_with_the_AIEP_program_in_general__6__`,How_satisfied_are_you_withUSATours_6_) %>% rowMeans(., na.rm = T)

evaluation_92 <- data.frame(eva_92$ID_encrypt, mean_score_courses, mean_score_on_personal_improvement, mean_score_on_program, mean_score_teachers, eva_92$Will_you_recommend_ALC_to_your_friends_6_)

write.csv(evaluation_92, "evaluation_92.csv")


############93

write_csv(eva_93, "eva_93.csv")

read_csv("eva_93.csv")

eva_93 = eva_93[eva_93$ID_encrypt != "NULL", ]

eva_93 = as.data.frame(eva_93)

eva_93 <- eva_93[,-c(9,33)]
eva_93 <- eva_93[,-c(16,24)]

for(i in 2:ncol(eva_93)){
  eva_93[,i] = as.numeric(eva_93[,i])
}

eva_93 = as.data.frame(cbind(eva_93$ID_encrypt, scale(eva_93[,2:38], center = T)))
names(eva_93)[1] = "ID_encrypt"

mean_score_teachers <- eva_93 %>% select(2,4,5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29) %>% rowMeans(., na.rm = T)

mean_score_courses <- eva_93 %>% select(My_general_evaluation_of_this_course_is_36_, My_general_evaluation_of_my_9_00_11_00_am_course_is_12_, My_general_evaluation_of_my_11_00_12_00_course_is_24_,My_general_evaluation_of_this_course_is_36_,My_general_evaluation_of_this_course_is_48_) %>% rowMeans(., na.rm = T)

mean_score_on_personal_improvement <- eva_93 %>% select(31,32,33,34,35) %>% rowMeans(., na.rm = T)

mean_score_on_program <- eva_93 %>% select(How_satisfied_are_you_with_the_AIEP_program_in_general_70_, How_satisfied_are_you_withUSATours_85_) %>% rowMeans(., na.rm = T)

evaluation_93 <- data.frame(eva_93$ID_encrypt, mean_score_courses, mean_score_on_personal_improvement, mean_score_on_program, mean_score_teachers, eva_93$Will_you_recommend_ALC_to_your_friends_91_)

write.csv(evaluation_93, "evaluation_93.csv")









