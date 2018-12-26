data <- read.csv("survey.csv")
library(data.table)
library(magrittr)
#outlier removal function
data$Age %>% unique() # observation has some non numeric rows
# Replacing non numeric rows to Na's
data$Age = as.numeric(as.character(data$Age)) 
# Removing the outliers in Age
replacing_rows = which(data$Age > 99 | data$Age < 1)
set(data,replacing_rows,"Age",NA)
# Converting 49 Gender levels to 3
# Gender levels Male, Female, Trans
library(stringr)
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

#for(i in 1:length(data)){
  aux <- prop.table(table(data$treatment, data[,13]), 1)*100 
  percent <- round(max(abs(aux[1,]-aux[2,])), digits = 2)
  
  if(percent > 10 & percent < 99){
    
    # Data preparing to visualization
    aux <- prop.table(table(data$treatment, data[,13]), 1)*100 
    nom <- colnames(aux)
    type <- c(rep("No",ncol(aux)),rep("Yes",ncol(aux)))
    val <- append(aux[1,], aux[2,])
    data.aux<-data.frame(nom=nom,type=type ,val=val)
    
    # Use of the library ggplot2 to data visualization 
    g <- ggplot() + geom_bar(data=data.aux,aes(x=nom, y=val,fill=type),stat='identity',position='dodge')+
      coord_flip() +
      labs(
        x = "Importance",
        y = "mental_illness",
        title = paste("Mental Health comparation about ", names(data[13]), sep=""),
        caption = "\nDetermined by matrix of covariances"
      )
    print(g)
  }
  
#}
