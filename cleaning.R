data <- read.csv("data14_16.csv")
str(data)



library(data.table)
library(magrittr)
library(stringr)
library(plyr)

# Removing Comments attribute
data <- data[,!names(data) %in% "comments"]


#outlier removal function
data$Age %>% unique() # observation has some non numeric rows
# Replacing non numeric rows to Na's
data$Age = as.numeric(as.character(data$Age)) 
# Removing the outliers in Age
replacing_rows = which(data$Age > 99 | data$Age < 1)
set(data,replacing_rows,"Age",NA)

# convert age to categorical variable
# 4 age groups
# 0-20, 20-35, 35-65, 65+
data$Age <- cut(data$Age, breaks=c(-Inf,20,35,65,Inf),labels=c("0-20","20-35","35-65","65+"))

# Converting 49 Gender levels to 3
# Gender levels Male, Female, Trans

data$Gender %<>% str_to_lower() %>% str_trim()
data$Gender %>% unique()

male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)","m|", "sex is male", "male.","make", "male ", "man","msle", "mail", "malr","cis man", "cis male","cisdude","dude")
trans_str <- c("trans-female","bigender", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby","transgender woman", "fluid","nonbinary","genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "nb masculine","male 9:1 female, roughly","queer", "mtf" ,"female or multi-gender femme","androgynous","other","genderfluid" ,"ostensibly male, unsure what that really means","genderfluid (born female)"  )
female_str <- c("cis female", "f","fem", "female","female/woman", "cisgender female","woman","fm",  "femake", "female ","cis-female/femme", "female (cis)", "femail","cis-woman","fem","cisgender female")
invalid_str <- c("p","a little about you","n/a","none of your business","human","afab")

data$Gender <- sapply(as.vector(data$Gender),function(x){
  if(x %in% male_str ){
    "male"
  } else if(x %in% female_str){
    "female"
  } else if(x %in% trans_str){
    "trans"
  } else {
    NA
  }
})
#converting Gender to Factor variable
data$Gender <- as.factor(data$Gender)


#converting Timestamp to Factor
data$Timestamp <- as.factor(data$Timestamp)

# no_employees
# 01-May is 1-5,Jun-25 is 6-25,More than 1000 is 1000+,'' is NA
levels(data$no_employees)[levels(data$no_employees)=="01-May"] <- "1-5"
levels(data$no_employees)[levels(data$no_employees)=="Jun-25"] <- "6-25"
levels(data$no_employees)[levels(data$no_employees)=="More than 1000"] <- "1000+"
levels(data$no_employees)[levels(data$no_employees)==""] <- NA
count(data$no_employees)

# State is valid only for USA 
# Converting all the state codes to Names

# To do string operations converting the state to char
data$state %<>% as.factor()
statecode_toname = function(x){
  if (is.null(x) || is.na(x) || x==""){
    NA
  }else if(nchar(x) == 2){
    state.name[match(x,state.abb)]
  }else{
    x
  }
}
data$state <- sapply(as.vector(data$state) ,statecode_toname)
data$state %<>% as.factor()


str(data)


# 0,1,Yes,No
# Self_employed,benefits,


change5 = function(x){
  if (is.null(x) || is.na(x) || x==""){
    NA
  }else if(x=="0"|x=="No"){
    "No"
  }else if(x=="1"|x=="Yes"){
    "Yes"
  }else if(x=="I am not sure"| x == "Not sure" | 
           x=="Don't know" | x=="I don't know" | x=="Maybe"){
    "Maybe"
  }else if(x=="Some of them"){
    "Some of them"
  }else{
    NA
  }
}


data$benefits <- sapply(as.vector(data$benefits),change5)
data$benefits %<>% as.factor()

data$self_employed <- sapply(as.vector(data$self_employed),change5)
data$self_employed %<>% as.factor()

data$mental_illness <-sapply(as.vector(data$mental_illness),change5)
data$mental_illness %<>% as.factor()

data$seek_help <- sapply(as.vector(data$seek_help),change5)
data$seek_help %<>% as.factor()

data$phys_health_consequence <- sapply(as.vector(data$phys_health_consequence),change5)
data$phys_health_consequence %<>% as.factor()

data$mental_health_consequence <- sapply(as.vector(data$mental_health_consequence),change5)
data$mental_health_consequence %<>% as.factor()

data$mental_vs_physical <- sapply(as.vector(data$mental_vs_physical),change5)
data$mental_vs_physical %<>% as.factor()

data$coworkers <- sapply(as.vector(data$coworkers),change5)
data$coworkers %<>% as.factor()

data$supervisor <- sapply(as.vector(data$supervisor),change5)
data$supervisor %<>% as.factor()

data$family_history <- sapply(as.vector(data$family_history),change5)
data$family_history %<>% as.factor()

data$tech_company <-sapply(as.vector(data$tech_company),change5)
data$tech_company %<>% as.factor()

data$care_options <- sapply(as.vector(data$care_options),change5)
data$care_options %<>% as.factor()

data$wellness_program <- sapply(as.vector(data$wellness_program),change5)
data$wellness_program %<>% as.factor()

data$mental_health_interview <- sapply(as.vector(data$mental_health_interview),change5)
data$mental_health_interview %<>% as.factor()

data$work_interfere <- sapply(as.vector(data$work_interfere),function(x){
  if(is.na(x) | is.null(x) | x == "") NA
  else if(x=="Never"| x=="Not applicable to me") "Never"
  else x
})
data$work_interfere %<>% as.factor()

data$remote_work <-sapply(as.vector(data$remote_work),function(x){
  if(is.na(x) | is.null(x) |x == "") NA
  else if(x=="Never" | x== "No") "No"
  else if(x=="Always"| x=="Yes") "Yes"
  else if(x=="Sometimes") "Sometimes"
  else NA
})
data$remote_work %<>% as.factor()

data$leave <- sapply(as.vector(data$leave),function(x){
  if(is.na(x) | is.null(x) | x=='') NA
  else if(x=="Don't know" | x== "I don't know") "Don't know"
  else x
})
data$leave %<>% as.factor()

str(data$mental_health_interview)
str(data)
count(data$mental_health_interview)
data$mental_illness%>% unique()

count(data$mental_illness)
nrow(data [as.vector(which(!is.na(data$state))),])
unused_attributes <- c("state","coworkers","supervisor","care_options")
temp <- data [,!names(data) %in% unused_attributes ]
temp <- temp[complete.cases(temp),]
sapply(data,function(x){ sum(is.na(x)) })



