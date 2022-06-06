
# recode questions
# 1 = open access
# 2 = open data, materials, and/or code
# 3 = preregistration
# 4 = open educational resources
# 5 = public engagement
# 6 = EUR support
# 7 = recognition and reward
# 8 = other
question_levels <-
  c(
    "1" = "In your opinion, how important is Open Access for your work?",
    "1" = "What is your experience with Open Access?",
    "1" = "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?",
    "1" = "Is there anything you want to share with us regarding your experiences with Open Access?", # ESL-only question
    "2" = "In your opinion, how important are open data, materials, and/or code for your work?",
    "2" = "What is your experience with using open data, materials, and/or code developed by others?",
    "2" = "What is your experience with openly sharing data, materials, and/or code that you developed?",
    "2" = "Are you familiar with the FAIR principles for data and code?",
    "2" = "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?",
    "3" = "In your opinion, how important is preregistration for your work?",
    "3" = "What is your experience with study preregistration?",
    "3" = "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with?",
    "4" = "In your opinion, how important are open educational resources for your work?",
    "4" = "What is your experience with using open educational resources developed by others?",
    "4" = "What is your experience with openly sharing educational resources that you developed?",
    "4" = "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with?",
    "5" = "In your opinion, how important is to have an open dialogue with society in your work?",
    "5" = "What is your experience engaging with society?",
    "5" = "The following are possible concerns that researchers could have about engaging with society. Which of these concerns would apply to you?",
    "6" = "Do you expect EUR to support you in learning open science practices?",
    "6" = "Which of the following open science practices would you like EUR to provide information or support for?",
    "6" = "What support services provided at EUR have you used to make your data FAIR?",
    "7" = "Do you feel recognized and rewarded by EUR (e.g. in the R&O cycle or appraisal conversation) for the Open Science activities you undertake?",
    "7" = "In what way were you recognized and rewarded?",
    "7" = "In what way do you expect to be recognized and rewarded?",
    "8" = "Is there anything else you would like to mention about Open Science practices?"
  )

school_levels <- c(
  "ESHCC" = "Erasmus School of History, Culture and Communication (ESHCC)",
  "ESHPM" = "Erasmus School of Health Policy & Management (ESHPM)",
  "ESPhil" = "Erasmus School of Philosophy (ESPHIL)",
  "ESSB" = "Erasmus School of Social and Behavioural Sciences (ESSB)",
  "ESL" = "Erasmus School of Law (ESL)",
  "ISS" = "International Institute of Social Studies (ISS)",
  "Other" = "Other_EUC", # too few responses
  "Other" = "Other_ESSB and ESHPM", # double affiliation coded as "Other"
  "Other" = "Other_I would rather not say" # non-disclosure coded as "Other"
)

department_levels <- c(
  "Other" = "Other_Erasmus SYNC Lab", # not a department
  "Other" = "Other_Strategy group: Communication and behavior change", # department?
  "Other" = "Other_Strategy Group",
  "Other" = "Other_Strategy",
  "Other" = "Other_strategy",
  "Other" = "Other_Support Staff",
  "Other" = "Public Administration,Sociology" # double affiliation coded as "Other"
)

position_levels <- c(
  "Lecturer" = "Other_Senior lecturer",
  "Lecturer" = "Other_lecturer",
  "Lecturer" = "Other_Lecturer",
  "Other" = "Other_Research assistant",
  "Other" = "Other_Employee",
  "Other" = "Other_Support",
  "Other" = "Other_Educational advisor",
  "Other" = "Other_Not sure why this matters",
  "Other" = "Other_I would rather not say"
)

# Likert_importance_convert <- c(
#   "1" = "Extremely important",
#   "2" = "Very important",
#   "3" = "Moderately important",
#   "4" = "Slightly important",
#   "5" = "Not at all important",
#   "6" = "I don’t know/prefer not to answer"
# )
# 
# Likert_experience_convert_Open_Access <- c(
#   "1" = "I have extensive experience with Open Access publishing",
#   "2" = "I have some experience with Open Access publishing",
#   "3" = "I am aware of Open Access publishing but have not done it",
#   "4" = "Until now, I was unaware of Open Access publishing",
#   "5" = "I don’t know/prefer not to answer"
# )
