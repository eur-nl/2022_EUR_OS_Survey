
# cluster 0: questions/school/department/position ----------------------------------------------------------------

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

# cluster 1: open access ----------------------------------------------------------------

open_access_Likert_importance_convert <- c(
  "1" = "Extremely important",
  "2" = "Very important",
  "3" = "Moderately important",
  "4" = "Slightly important",
  "5" = "Not at all important",
  "6" = "I don’t know/prefer not to answer"
)

open_access_Likert_experience_convert <- c(
  "1" = "I have extensive experience with Open Access publishing",
  "2" = "I have some experience with Open Access publishing",
  "3" = "I am aware of Open Access publishing but have not done it",
  "4" = "Until now, I was unaware of Open Access publishing",
  "5" = "I don’t know/prefer not to answer"
)

open_access_Likert_concerns_convert <- c(
  "Some journals might not publish findings that are uploaded to a pre-publication archive" = "Green OA (self-archiving): Some journals might not publish findings that are uploaded to a pre-publication archive",
  "Other people might copy my research and publish it before I do" = "Green OA (self-archiving): Other people might copy my research and publish it before I do",
  "Non-peer-reviewed findings might add noise to the literature" = "Green OA (self-archiving): Non-peer-reviewed findings might add noise to the literature",
  "Making my work available pre-publication might reduce the number of citations to the ultimately published work" = "Green OA (self-archiving): Making my work available pre-publication might reduce the number of citations to the ultimately published work",
  "Availability of the pre-publication manuscript might highlight differences (e.g., errors in analysis, revisions to hypotheses) between the original conception of the research and the ultimately published work" = "Green OA (self-archiving): Availability of the pre-publication manuscript might highlight differences (e.g., errors in analysis, revisions to hypotheses) between the original conception of the research and the ultimately published work",
  "Open Access journals might have lower quality articles" = "Gold OA: Open Access journals might have lower quality articles",
  "Open Access journals might not provide rigorous peer-review" = "Gold OA: Open Access journals might not provide rigorous peer-review",
  "I might not have enough funding to pay Article Processing Charges for Open Access journals" = "Gold OA: I might not have enough funding to pay Article Processing Charges for Open Access journals",
  "The impact of an Open Access publication might be low (e.g., few downloads and citations, low public engagement)" = "Gold OA: The impact of an Open Access publication might be low (e.g., few downloads and citations, low public engagement)",
  "I do not share any of these concerns" = "I do not share any of these concerns",
  "I don’t know/prefer not to answer" = "I don’t know/prefer not to answer",
  "Other" = "Other_For me the most important issue is where to find publications. And if this place/website is clear that publications can be \"googeld\" easily."
)

# cluster 2: open data, materials, and/or code ----------------------------------------------------------------

open_data_materials_code_Likert_importance_convert <- c(
  "1" = "Extremely important",
  "2" = "Very important",
  "3" = "Moderately important",
  "4" = "Slightly important",
  "5" = "Not at all important",
  "6" = "I don’t know/prefer not to answer"
)

open_data_materials_code_Likert_experience_others_convert <- c(
  "1" = "I regularly use open data, materials, and/or code developed by others",
  "2" = "I have some experience with open data, materials, and/or code developed by others, but do not use them regularly",
  "3" = "I am aware of open data, materials, and/or code developed by others, but have not used them",
  "4" = "Until now, I hadn't heard of open data, materials, and/or code",
  "5" = "I don’t know/prefer not to answer"
)

open_data_materials_code_Likert_experience_own_convert <- c(
  "1" = "I regularly share open data, materials, and/or code",
  "2" = "I have some experience with open data, materials, and/or code, but do not share mine regularly",
  "3" = "I am aware of open data, materials, and/or code, but have not shared my own",
  "4" = "Until now, I hadn't heard of open data, materials, and/or code",
  "5" = "I don’t know/prefer not to answer"
)

open_data_materials_code_Likert_FAIR_convert <- c(
  "1" = "I regularly follow the FAIR principles",
  "2" = "I have some experience with the FAIR principles, but do not follow them regularly",
  "3" = "I am aware of the FAIR principles, but have not followed them",
  "4" = "Until now, I hadn't heard of the FAIR principles",
  "5" = "I don’t know/prefer not to answer"
)

open_data_materials_code_Likert_concerns_convert <- c(
  "Other" = "Other_I do not 'own' the data and I'm not allowed to share the data",
  "Other" = "Other_I do not research in which this is relevant.",
  "Other" = "Other_Sharing data is not always a good idea when for example one works with vulnerable groups and informants might be at risk of stigma, prosecution, etc.",
  "Other" = "Other_The data I use comes from a third party which doesn't allow me to share the data."
)

# cluster 3: preregistration ----------------------------------------------------------------

preregistration_Likert_importance_convert <- c(
  "1" = "Extremely important",
  "2" = "Very important",
  "3" = "Moderately important",
  "4" = "Slightly important",
  "5" = "Not at all important",
  "6" = "I don’t know/prefer not to answer"
)

preregistration_Likert_experience_convert <- c(
  "1" = "I regularly preregister my studies",
  "2" = "I have some experience with study preregistration, but do not use it regularly",
  "3" = "I am aware of study preregistration, but have not used it",
  "4" = "Until now, I was unaware of study preregistration",
  "5" = "I don’t know/prefer not to answer"
)

preregistration_Likert_concerns_convert <- c(
  "Other" = "Other_Preregistration might work and may be the right thing to do for some type of studies but not for others.",
  "Other" = "Other_I do not do research in which this is relevant.",
  "Other" = "Other_It presents a positivistic view of science that is v useful for quantitative, but less for qualitative research",
  "Other" = "Other_Not applicable to my qualitative work!"
)

# cluster 4: open educational resources ----------------------------------------------------------------

open_education_Likert_importance_convert <- c(
  "1" = "Extremely important",
  "2" = "Very important",
  "3" = "Moderately important",
  "4" = "Slightly important",
  "5" = "Not at all important",
  "6" = "I don’t know/prefer not to answer"
)

open_education_Likert_experience_others_convert <- c(
  "1" = "I regularly use open educational resources developed by others",
  "2" = "I have some experience with open educational resources developed by others, but do not use them regularly",
  "3" = "I am aware of open educational resources developed by others, but have not used them",
  "4" = "Until now, I hadn't heard of open educational resources",
  "5" = "I don’t know/prefer not to answer"
)

open_education_Likert_experience_own_convert <- c(
  "1" = "I regularly share open educational resources",
  "2" = "I have some experience with open educational resources, but do not share mine regularly",
  "3" = "I am aware of open educational resources, but have not shared my own",
  "4" = "Until now, I hadn't heard of open educational resources",
  "5" = "I don’t know/prefer not to answer"
)

open_education_Likert_concerns_convert <- c(
  "Other" = "Other_I don't know where to share educational resources I have created",
  "Other" = "Other_my data is often highly sensitive and hard to anonymise. sharing would violate ethical principles"
)

# cluster 5: societal engagement ----------------------------------------------------------------

societal_engagement_Likert_importance_convert <- c(
  "1" = "Extremely important",
  "2" = "Very important",
  "3" = "Moderately important",
  "4" = "Slightly important",
  "5" = "Not at all important",
  "6" = "I don’t know/prefer not to answer"
)

societal_engagement_Likert_experience_convert <- c(
  "1" = "I regularly engage with society",
  "2" = "I have some experience with engaging with society, but do not do it regularly",
  "3" = "I am aware of engaging with society, but have not done it",
  "4" = "Until now, I was unaware of open engagement with societal actors",
  "5" = "I don’t know/prefer not to answer"
)

societal_engagement_Likert_concerns_convert <- c(
  "Other" = "Other_I find it difficult to address certain issues in the media afraid of a backless from certain groups of people.",
  "Other" = "Other_It is not relevant to my research",
  "Other" = "Other_Most of the societies I engage with is past us."
)

# cluster 6: EUR support ----------------------------------------------------------------

Likert_EUR_support_convert <- c(
  "1" = "Yes",
  "2" = "No",
  "3" = "I don’t know/prefer not to answer"
)

# cluster 7: recognition and rewards ----------------------------------------------------------------

Likert_feeling_recognized_convert <- c(
  "1" = "Yes",
  "2" = "No",
  "3" = "I do not undertake any Open Science activities",
  "4" = "I don’t know/prefer not to answer"
)

Likert_current_recognition_convert <-c(
  "Other" =  "Other_OA publications are recognized as relevant output",
  "Other" = "Other_Republication of my community outreach articles"
)

Likert_expected_recognition_convert <-c(
  "Other" = "Other_I do not really expect specific recognition or reward for 'open work', and indeed that recognition and reward is not present",
  "Other" = "Other_I don't expect the eur to award",
  "Other" = "Other_If this is important, it should be recognized and rewarded. I don't really care how, but now there is no attention for these practices whatsoever.",
  "Other" = "Other_None, it should become common practice",
  "Other" = "Other_should be 'counted' just like articles. Especially given that data gets its own DOI",
  "Other" = "Other_To be honest, sharing data & methods is just a part of the publication cycle, no need for it to be rewarded separately",
  "Other" = "Other_Value not just 'hard' academic output but also societal engagement"
)

# cluster 8: other ----------------------------------------------------------------

# no recoding, only free text






