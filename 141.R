names(Dashboard)[1] = "Custom_Data_1"
names(Elective17)
merged <- merge(Dashboard_24_export, Program_Evaluations_34_export, by = "ID_encrypt")
#######
Placemen6$Custom_Data_1 = substring(Placemen6$Custom_Data_1, 2)
names(Placement_Finals_16)[10] = "Custom_Data_1"
Placement_Finals_16$Custom_Data_1 = substring(Placement_Finals_16$Custom_Data_1, 3)

Elective20 = Elective20[, -c(5,6,7)]
Elective20 = Elective20[, -6]



Placementfinal16 <- data.frame(Placement_Finals_16$Custom_Data_1, Placement_Finals_16$`_COURSE_NO__`, Placement_Finals_16$ABSENCES, Placement_Finals_16$GRADE, Placement_Finals_16$GENDER)
Placementfinal16 = Placementfinal16[Placementfinal16$Placement_Finals_16.GRADE != "NULL" & Placementfinal16$Placement_Finals_16.GRADE != "Do Not Record" & Placementfinal16$Placement_Finals_16.GRADE != "W"
                                    & Placementfinal16$Placement_Finals_16.GRADE != "Complete" & Placementfinal16$Placement_Finals_16.GRADE != "DR",]




barplot(Placementfinal16)
library(dplyr)
newplacefinal16 <- Placementfinal16 %>% group_by(Custom_Data_1) 




names(Placemen6)[1] = "Custom_Data_1"



################Clean Evaluation 34#################




Program_Evaluations_34_export <- Program_Evaluations_34_export[-c(295,418), ]

Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_ =  as.factor(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_)
levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_)[levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_)
levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_)[levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_) == "I strongly agree"] <- 3
levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_)[levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_) == "I somewhat agree"] <- 2
levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_)[levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_) == "I somewhat disagree"] <- 1
levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_)[levels(Program_Evaluations_34_export$This_teacher_is_organized_and_prepared_1_) == "I strongly disagree"] <- 0

Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_ = as.factor(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_) == "A - Very good"] <- 4
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_) == "B - Somewhat good"] <- 3
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_) == "C - Good"] <- 2
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_) == "D - Somewhat bad"] <- 1
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_teacher_is_1_) == "E - Very bad"] <- 0

Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_ = as.factor(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "A - Very good"] <- 4
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "B - Somewhat good"] <- 3
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "C - Good"] <- 2
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "D - Somewhat bad" | levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "C - Somewhat bad" ] <- 1
levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_)[levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "E - Very bad" | levels(Program_Evaluations_34_export$My_general_evaluation_of_this_course_is_1_) == "E - Very bad" ] <- 0

Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_ = as.factor(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_)
levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_)
levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_) == "I somewhat agree"] <- 2
levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_) == "I somewhat disagree"] <- 1
levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_) == "I strongly agree"] <- 3
levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_interesting_activities_1_) == "I strongly disagree"] <- 0


Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_ = as.factor(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_)
levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_)[levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_)
levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_)[levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_) == "I somewhat agree"] <- 2
levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_)[levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_) == "I somewhat disagree"] <- 1
levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_)[levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_) == "I strongly agree"] <- 3
levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_)[levels(Program_Evaluations_34_export$This_teacher_explains_things_clearly_1_) == "I strongly disagree"] <- 0

Program_Evaluations_34_export$This_teacher_answers_questions_well_1_ = as.factor(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_)
levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_)[levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_)
levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_)[levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_) == "I somewhat agree"] <- 2
levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_)[levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_) == "I somewhat disagree"] <- 1
levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_)[levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_) == "I strongly agree"] <- 3
levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_)[levels(Program_Evaluations_34_export$This_teacher_answers_questions_well_1_) == "I strongly disagree"] <- 0

Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_ = as.factor(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I somewhat agree"] <- 2
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I somewhat disagree"] <- 1
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I strongly agree"] <- 3
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I strongly disagree"] <- 0

Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_ = as.factor(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "NULL"] <- NA
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I somewhat agree"] <- 2
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I somewhat disagree"] <- 1
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I strongly agree"] <- 3
levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_)[levels(Program_Evaluations_34_export$This_teacher_gives_students_chances_to_participate_in_class_1_) == "I strongly disagree"] <- 0






eva_33 <- as.data.frame(Program_Evaluations_33_export[, c(1, 5:11,17:23)])


for(i in 2:ncol(eva_33)){
  eva_33[,i] = as.numeric(eva_33[,i])
}

levels(eva_33[,2])
levels(eva_33[,2])[levels(eva_33[,2]) == "NULL"] <- NA

for(i in 2:ncol(eva_33)){
  levels(eva_33[,i])[levels(eva_33[,i]) == "NULL"] <- NA
  levels(eva_33[,i])[levels(eva_33[,i]) == "I strongly agree"] <- 3
  levels(eva_33[,i])[levels(eva_33[,i]) == "I somewhat agree"] <- 2
  levels(eva_33[,i])[levels(eva_33[,i]) == "I somewhat disagree"] <- 1
  levels(eva_33[,i])[levels(eva_33[,i]) == "I strongly disagree"] <- 0
  levels(eva_33[,i])[levels(eva_33[,i]) == "Very good"] <- 4
  levels(eva_33[,i])[levels(eva_33[,i]) == "Somewhat good"] <- 3
  levels(eva_33[,i])[levels(eva_33[,i]) == "Good"] <- 2
  levels(eva_33[,i])[levels(eva_33[,i]) == "Somewhat bad"] <- 1
  levels(eva_33[,i])[levels(eva_33[,i]) == "E - Very bad"] <- 0
  levels(eva_33[,i])[levels(eva_33[,i]) == "Very satisfied"] <- 4
  levels(eva_33[,i])[levels(eva_33[,i]) == "Satisfied"] <- 3
  levels(eva_33[,i])[levels(eva_33[,i]) == "Somewhat satisfied"] <- 2
  levels(eva_33[,i])[levels(eva_33[,i]) == "Somewhat dissatisfied"] <- 1
  levels(eva_33[,i])[levels(eva_33[,i]) == "Disstatisfied"] <- 0
  levels(eva_33[,i])[levels(eva_33[,i]) == "A"] <- 4
  levels(eva_33[,i])[levels(eva_33[,i]) == "B"] <- 3
  levels(eva_33[,i])[levels(eva_33[,i]) == "C"] <- 2
  levels(eva_33[,i])[levels(eva_33[,i]) == "D"] <- 1
  levels(eva_33[,i])[levels(eva_33[,i]) == "A - Very good"] <- 3
  levels(eva_33[,i])[levels(eva_33[,i]) == "B - Somewhat good"] <- 2
  levels(eva_33[,i])[levels(eva_33[,i]) == "C - Good"] <- 1
  levels(eva_33[,i])[levels(eva_33[,i]) == "A- very satisfied"] <- 4
  levels(eva_33[,i])[levels(eva_33[,i]) == "B- somewhat satisfied"] <- 3
  levels(eva_33[,i])[levels(eva_33[,i]) == "E- did not use"] <- 0
  levels(eva_33[,i])[levels(eva_33[,i]) == "Yes"] <- 1
  levels(eva_33[,i])[levels(eva_33[,i]) == "No"] <- 0
}


eva_34 <- as.data.frame(Program_Evaluations_34_export[, c(1,7:13, 19:25)])

eva_34 <- as.data.frame(eva_34)
for(i in 2:ncol(eva_34)){
  eva_34[,i] = as.numeric(eva_34[,i])
}

for(i in 2:ncol(eva_34)){
  levels(eva_34[,i])[levels(eva_34[,i]) == "NULL"] <- NA
  levels(eva_34[,i])[levels(eva_34[,i]) == "I strongly agree"] <- 3
  levels(eva_34[,i])[levels(eva_34[,i]) == "I somewhat agree"] <- 2
  levels(eva_34[,i])[levels(eva_34[,i]) == "I somewhat disagree"] <- 1
  levels(eva_34[,i])[levels(eva_34[,i]) == "I strongly disagree"] <- 0
  levels(eva_34[,i])[levels(eva_34[,i]) == "Very good"] <- 4
  levels(eva_34[,i])[levels(eva_34[,i]) == "Somewhat good"] <- 3
  levels(eva_34[,i])[levels(eva_34[,i]) == "Good"] <- 2
  levels(eva_34[,i])[levels(eva_34[,i]) == "Somewhat bad"] <- 1
  levels(eva_34[,i])[levels(eva_34[,i]) == "Very bad"] <- 0
  levels(eva_34[,i])[levels(eva_34[,i]) == "Very satisfied"] <- 4
  levels(eva_34[,i])[levels(eva_34[,i]) == "Satisfied"] <- 3
  levels(eva_34[,i])[levels(eva_34[,i]) == "Somewhat satisfied"] <- 2
  levels(eva_34[,i])[levels(eva_34[,i]) == "Somewhat dissatisfied"] <- 1
  levels(eva_34[,i])[levels(eva_34[,i]) == "Disstatisfied"] <- 0
  levels(eva_34[,i])[levels(eva_34[,i]) == "A"] <- 4
  levels(eva_34[,i])[levels(eva_34[,i]) == "B"] <- 3
  levels(eva_34[,i])[levels(eva_34[,i]) == "C"] <- 2
  levels(eva_34[,i])[levels(eva_34[,i]) == "D"] <- 1
  levels(eva_34[,i])[levels(eva_34[,i]) == "A- a lot"] <- 3
  levels(eva_34[,i])[levels(eva_34[,i]) == "B- some"] <- 2
  levels(eva_34[,i])[levels(eva_34[,i]) == "C- not much"] <- 1
  levels(eva_34[,i])[levels(eva_34[,i]) == "A- very satisfied"] <- 4
  levels(eva_34[,i])[levels(eva_34[,i]) == "B- somewhat satisfied"] <- 3
  levels(eva_34[,i])[levels(eva_34[,i]) == "E- did not use"] <- 0
  levels(eva_34[,i])[levels(eva_34[,i]) == "Yes"] <- 1
  levels(eva_34[,i])[levels(eva_34[,i]) == "No"] <- 0
  levels(eva_34[,i])[levels(eva_34[,i]) == "A - Very good"] <- 4
  levels(eva_34[,i])[levels(eva_34[,i]) == "B - Somewhat good"] <- 3
  levels(eva_34[,i])[levels(eva_34[,i]) == "C - Good"] <- 2
  levels(eva_34[,i])[levels(eva_34[,i]) == "D - Somewhat bad" | levels(eva_34[,i]) == "C - Somewhat bad" ] <- 1
  levels(eva_34[,i])[levels(eva_34[,i]) == "D - Very bad"  ] <- 0
}

##################################

eva_46 <- as.data.frame(Program_Evaluations_46_export[, c(1,7:13, 19:25)])

for(i in 2:ncol(eva_46)){
  eva_46[,i] = as.factor(eva_46[,i])
}

for(i in 2:ncol(eva_46)){
  levels(eva_46[,i])[levels(eva_46[,i]) == "NULL"] <- NA
  levels(eva_46[,i])[levels(eva_46[,i]) == "I strongly agree"] <- 3
  levels(eva_46[,i])[levels(eva_46[,i]) == "I somewhat agree"] <- 2
  levels(eva_46[,i])[levels(eva_46[,i]) == "I somewhat disagree"] <- 1
  levels(eva_46[,i])[levels(eva_46[,i]) == "I strongly disagree"] <- 0
  levels(eva_46[,i])[levels(eva_46[,i]) == "Very good"] <- 4
  levels(eva_46[,i])[levels(eva_46[,i]) == "Somewhat good"] <- 3
  levels(eva_46[,i])[levels(eva_46[,i]) == "Good"] <- 2
  levels(eva_46[,i])[levels(eva_46[,i]) == "Somewhat bad"] <- 1
  levels(eva_46[,i])[levels(eva_46[,i]) == "Very bad"] <- 0
  levels(eva_46[,i])[levels(eva_46[,i]) == "Very satisfied"] <- 4
  levels(eva_46[,i])[levels(eva_46[,i]) == "Satisfied"] <- 3
  levels(eva_46[,i])[levels(eva_46[,i]) == "Somewhat satisfied"] <- 2
  levels(eva_46[,i])[levels(eva_46[,i]) == "Somewhat dissatisfied"] <- 1
  levels(eva_46[,i])[levels(eva_46[,i]) == "Disstatisfied"] <- 0
  levels(eva_46[,i])[levels(eva_46[,i]) == "A"] <- 4
  levels(eva_46[,i])[levels(eva_46[,i]) == "B"] <- 3
  levels(eva_46[,i])[levels(eva_46[,i]) == "C"] <- 2
  levels(eva_46[,i])[levels(eva_46[,i]) == "D"] <- 1
  levels(eva_46[,i])[levels(eva_46[,i]) == "A- a lot"] <- 3
  levels(eva_46[,i])[levels(eva_46[,i]) == "B- some"] <- 2
  levels(eva_46[,i])[levels(eva_46[,i]) == "C- not much"] <- 1
  levels(eva_46[,i])[levels(eva_46[,i]) == "A- very satisfied"] <- 4
  levels(eva_46[,i])[levels(eva_46[,i]) == "B- somewhat satisfied"] <- 3
  levels(eva_46[,i])[levels(eva_46[,i]) == "E- did not use"] <- 0
  levels(eva_46[,i])[levels(eva_46[,i]) == "Yes"] <- 1
  levels(eva_46[,i])[levels(eva_46[,i]) == "No"] <- 0
  levels(eva_46[,i])[levels(eva_46[,i]) == "A - Very good"] <- 4
  levels(eva_46[,i])[levels(eva_46[,i]) == "B - Somewhat good"] <- 3
  levels(eva_46[,i])[levels(eva_46[,i]) == "C - Good"] <- 2
  levels(eva_46[,i])[levels(eva_46[,i]) == "D - Somewhat bad" | levels(eva_34[,i]) == "C - Somewhat bad" ] <- 1
  levels(eva_46[,i])[levels(eva_46[,i]) == "E - Very bad" | levels(eva_34[,i]) == "E - Very bad" ] <- 0
}







##################################

final_16 <- data.frame(Placement_Finals_16_export$ID_encrypt,
                    Placement_Finals_16_export$ABSENCES, Placement_Finals_16_export$GRADE, Placement_Finals_16_export$GENDER, Placement_Finals_16_export$LEVEL, Placement_Finals_16_export$CITIZENSHIP)

names(final_16) <- c("ID_ encrypt",  "Absences", "Grade", "Gender", "Level", "Citizenship")



final_16$Grade = as.factor(final_16$Grade)


levels(final_16$Grade)[levels(final_16$Grade) == "A"] <- 4
levels(final_16$Grade)[levels(final_16$Grade) == "A+"] <- 4
levels(final_16$Grade)[levels(final_16$Grade) == "A-"] <- 3.7
levels(final_16$Grade)[levels(final_16$Grade) == "B"] <- 3
levels(final_16$Grade)[levels(final_16$Grade) == "B+"] <- 3.3
levels(final_16$Grade)[levels(final_16$Grade) == "B-"] <- 2.7
levels(final_16$Grade)[levels(final_16$Grade) == "C+"] <- 2.3
levels(final_16$Grade)[levels(final_16$Grade) == "C"] <- 2
levels(final_16$Grade)[levels(final_16$Grade) == "C-"] <- 1.7
levels(final_16$Grade)[levels(final_16$Grade) == "D" | levels(final_16$Grade) == "D+" | levels(final_16$Grade) == "D-" | levels(final_16$Grade) == "F"] <- 0
levels(final_16$Grade)[levels(final_16$Grade) == "Do Not Complete"] = "NP"
levels(final_16$Grade)[levels(final_16$Grade) == "Complete"] = "P"
levels(final_16$Grade)[levels(final_16$Grade) == "W"] = "NP"
final_16 = final_16[final_16$Grade != "DR" & final_16$Grade != "Do Not Record" & final_16$Grade != "NULL", ]

names(final_16)[1] = "ID_encrypt"

write.csv(final_16, "final_16.csv")

read_csv("final_16.csv")

final_16 = final_16[,-2]

absenses <- final_16 %>% group_by(ID_encrypt) %>% summarise(Absences = sum(Absences))

count_PNP <- final_25 %>% group_by(ID_encrypt) %>% summarise(PNP_ratio = (count(Grade == "P"))/(count(Grade)))

final_16_gpa <- final_16[final_16$Grade == "4" | final_16$Grade == "3.7" | final_16$Grade == "3" | final_16$Grade == "3.3" | final_16$Grade == "2.7" | final_16$Grade == "2.3" | final_16$Grade == "2" | final_16$Grade == "1.7" | final_16$Grade == "0",]

GPA <- final_16_gpa %>% group_by(ID_encrypt) %>% summarise(GPA = mean(as.numeric(as.character(Grade))))

PNP <- final_16[final_16$Grade == "P" | final_16$Grade == "NP", ]

PNP_1 <- PNP %>% group_by(ID_encrypt) %>% summarise(count_P = sum(Grade == "P"), count_NP = sum(Grade == "NP"))

PNP_1$Pass_Ratio = PNP_1$count_P / (PNP_1$count_P+PNP_1$count_NP)

PNP_1 = PNP_1[,c(1,4)]


final_16_cleaned <- merge(GPA, absenses, by = "ID_encrypt", all = T)

final_16_cleaned <- merge(final_16_cleaned, PNP_1, by = "ID_encrypt", all = T)

write_csv(final_16_cleaned, "final_16_cleaned.csv")


##################################

final_25 <- data.frame(Placement_Finals_25_export$ID_encrypt,
                       Placement_Finals_25_export$ABSENCES, Placement_Finals_25_export$GRADE, Placement_Finals_25_export$GENDER, Placement_Finals_25_export$LEVEL, Placement_Finals_25_export$CITIZENSHIP)

names(final_25) <- c("ID_ encrypt",  "Absences", "Grade", "Gender", "Level", "Citizenship")


final_25$Grade = as.factor(final_25$Grade)


levels(final_25$Grade)[levels(final_25$Grade) == "A"] <- 4
levels(final_25$Grade)[levels(final_25$Grade) == "A+"] <- 4
levels(final_25$Grade)[levels(final_25$Grade) == "A-"] <- 3.7
levels(final_25$Grade)[levels(final_25$Grade) == "B"] <- 3
levels(final_25$Grade)[levels(final_25$Grade) == "B+"] <- 3.3
levels(final_25$Grade)[levels(final_25$Grade) == "B-"] <- 2.7
levels(final_25$Grade)[levels(final_25$Grade) == "C+"] <- 2.3
levels(final_25$Grade)[levels(final_25$Grade) == "C"] <- 2
levels(final_25$Grade)[levels(final_25$Grade) == "C-"] <- 1.7
levels(final_25$Grade)[levels(final_25$Grade) == "D" | levels(final_25$Grade) == "D+" | levels(final_25$Grade) == "D-" | levels(final_16$Grade) == "F"] <- 0
levels(final_25$Grade)[levels(final_25$Grade) == "Do Not Complete"] = "NP"
levels(final_25$Grade)[levels(final_25$Grade) == "Complete"] = "P"
levels(final_25$Grade)[levels(final_25$Grade) == "W"] = "NP"
final_25 = final_25[final_25$Grade != "DR" & final_25$Grade != "Do Not Record" & final_25$Grade != "NULL", ]

names(final_25)[1] = "ID_encrypt"

write.csv(final_25, "final_16.csv")

final_25 = final_25[,-2]

absenses <- final_25 %>% group_by(ID_encrypt) %>% summarise(Absences = sum(Absences))


final_25_gpa <- final_25[final_25$Grade == "4" | final_25$Grade == "3.7" | final_25$Grade == "3" | final_25$Grade == "3.3" | final_25$Grade == "2.7" | final_25$Grade == "2.3" | final_25$Grade == "2" | final_25$Grade == "1.7" | final_25$Grade == "0",]

GPA <- final_25_gpa %>% group_by(ID_encrypt) %>% summarise(GPA = mean(as.numeric(as.character(Grade))))

PNP <- final_25[final_25$Grade == "P" | final_25$Grade == "NP", ]

levels(PNP$Grade)[levels()]
PNP_1 <- PNP %>% group_by(ID_encrypt) %>% summarise(count_P = sum(Grade == "P"), count_NP = sum(Grade == "NP"))

PNP_1$Pass_Ratio = PNP_1$count_P / (PNP_1$count_P+PNP_1$count_NP)

PNP_1 = PNP_1[,c(1,4)]

final_25_cleaned <- merge(GPA, absenses, by = "ID_encrypt", all = T)

final_25_cleaned <- merge(final_25_cleaned, PNP_1, by = "ID_encrypt", all = T)

write_csv(final_25_cleaned, "final_25_cleaned.csv")

typeof(final_16_cleaned$ID_encrypt)

final_16_cleaned$ID_encrypt[1:10]

