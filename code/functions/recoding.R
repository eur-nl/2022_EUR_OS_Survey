recode_all <-
  c(
    # recode school
    "ESHCC" = "Erasmus School of History, Culture and Communication (ESHCC)",
    "ESHPM" = "Erasmus School of Health Policy & Management (ESHPM)",
    "ESPhil" = "Erasmus School of Philosophy (ESPHIL)",
    "ESSB" = "Erasmus School of Social and Behavioural Sciences (ESSB)",
    "ESL" = "Erasmus School of Law (ESL)",
    "ISS" = "International Institute of Social Studies (ISS)",
    "Other" = "EUC", # too few responses
    "Other" = "ESSB and ESHPM", # double affiliation coded as "Other"
    "Other" = "I would rather not say", # non-disclosure coded as "Other"
    # recode position
    "Lecturer" = "Senior lecturer",
    "Lecturer" = "lecturer",
    "Other" = "Research assistant",
    "Other" = "Employee",
    "Other" = "Support",
    "Other" = "Support Staff",
    "Other" = "Educational advisor",
    "Other" = "Not sure why this matters",
    # recode department
    "Other" = "Erasmus SYNC Lab", # not a department
    "Other" = "Strategy group: Communication and behavior change", # department?
    "Other" = "Strategy Group",
    "Other" = "Strategy",
    "Other" = "strategy",
    "Other" = "Public Administration,Sociology" # double affiliation coded as "Other"
  )
