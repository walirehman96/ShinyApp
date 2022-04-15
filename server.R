library('shiny')
library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library("DT")
library('dplyr')
library('markdown')
library('zoo')
library('tidyr')
library('rworldmap')
library('rnaturalearth')
library('rnaturalearthdata')
library('ggplot2')
library('tidyverse')


df=read.csv("/Users/walirehman96/Desktop/Final Term (100369726)/ShinyFinal/CPSCFinal/survey.csv")
head(df)

str(df)


sum(is.na(df))

df$state=df$state %>% replace_na("NA")

sum(is.na(df))

df$comments=NULL

df$work_interfere=df$work_interfere %>% replace_na("NA")
df$self_employed=df$self_employed %>% replace_na("NA")

sum(is.na(df))
str(df)


table(df['no_employees'])
table(df['Gender'])

df$Gender=as.character(df$Gender)
df$Gender=tolower(df$Gender)

Female=c('female', 'cis female', 'f', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail')

Male=c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 
          'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man','Mle')

Others=c('others','a little about you','agender','androgyne','queer/she/they', 'non-binary', 'nah', 'enby', 'fluid', 'genderqueer',  
            'guy (-ish) ^_^', 'male leaning androgynous', 'neuter', 'queer','p','all','trans-female', 'trans woman',
            'female (trans)','something kinda male?','ostensibly male, unsure what that really means')


Gender_new=as.vector(df$Gender)  

Gender_new=sapply(as.vector(Gender_new), function(x) if(x %in% Male) "Male" else x)
Gender_new=sapply(as.vector(Gender_new), function(x) if(x %in% Female) "Female" else x)
Gender_new=sapply(as.vector(Gender_new), function(x) if(x %in% Others) "Others" else x)

df$Gender<- Gender_new


table(df['Gender'])




df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#Removing outliers
df<-df[!(df$Age<0 | df$Age >100),]


#FOr histogram
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()




shinyServer(function(input, output) {
  
  output$typeSelectOutput=renderUI({
    selectInput("typeInput","Select country:",
                sort(unique(df$Country)),
                multiple = TRUE,
                selected = c("United States","India", "Netherlands", "Belgium","Brazil", "France", "Hungary","Mexico"))
    
  })
  
  output$thePlot=renderPlot({
    df$no_employees=as.factor(df$no_employees)
    if(input$gender == "female"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        ggplot(aes(x = no_employees, fill = benefits)) + geom_bar(position="fill", fill='green') +
        xlab("No of employees") + 
        ylab(paste0("benefit proportion for", input$gender)) + ggtitle(paste0("Does you employeer provide you mental health benefits to",input$gender))
        
    }
    else if(input$gender == "male"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male") %>%
        ggplot(aes(x = no_employees, fill = benefits)) + geom_bar(position="fill", fill='red') +
        xlab("No of employees") + 
        ylab(paste0("benefit proportion for", input$gender)) + ggtitle(paste0("Does you employeer provide you mental health benefits to",input$gender))
      
      
    }
    else{
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others") %>%
        ggplot(aes(x = no_employees, fill = benefits)) + geom_bar(position="fill")  +
        xlab("No of employees") + 
        ylab(paste0("benefit proportion for", input$gender)) + ggtitle(paste0("Does you employeer provide you mental health benefits to",input$gender))
      
      
    }
    plot
    
  })    
  
  
  output$seekhelp=renderPlot({
    df$seek_help=as.factor(df$seek_help)
    if(input$gender == "female"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        filter(no_employees == input$companysize) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge") +
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to seek help")) 
    }
    else if(input$gender == "male"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2])%>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male" & no_employees == input$company_size) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to seek help")) 
      
    }
    else{
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others" & no_employees == input$company_size) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to seek help")) 
      
    }
    plot
    
  })    
  

  output$companysize=renderPlot({
    
    if(input$gender == "female"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        ggplot(aes(x=no_employees,y=treatment, fill=factor(tech_company)), color=factor(vs)) +  
        stat_summary(fun=mean,position=position_dodge(),geom="bar")+
        xlab("No of employees") + 
        ylab(paste0("Treatment proportion for", input$gender)) 
    }
    else if(input$gender == "male"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2])%>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male") %>%
        ggplot(aes(x=no_employees,y=treatment, fill=factor(tech_company)), color=factor(vs)) +  
        stat_summary(fun=mean,position=position_dodge(),geom="bar")+
        xlab("No of employees") + 
        ylab(paste0("Treatment proportion for", input$gender)) 
      
    }
    else{
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others") %>%
        ggplot(aes(x=no_employees,y=treatment, fill=factor(tech_company)), color=factor(vs)) +  
        stat_summary(fun=mean,position=position_dodge(),geom="bar")+
        xlab("No of employees") + 
        ylab(paste0("Treatment proportion for", input$gender)) 
      
    }
    plot
    
  }) 
  
  output$mentalhealth=renderPlot({
    if(input$gender == "female"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        filter(no_employees == input$companysize) %>%
        filter(tech_company ==input$company) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "darkgreen")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to disscuss the mental health concern")) 
    }
    else if(input$gender == "male"){
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2])%>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male" & no_employees == input$company_size) %>%
        filter(tech_company ==input$company) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "darkgreen")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to disscuss the mental health concern")) 
      
    }
    else{
      plot=df %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others" & no_employees == input$company_size) %>%
        filter(tech_company == input$company) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "darkgreen")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to disscuss the mental health concern")) 
      
    }
    plot
    
  })    
  
  
  output$country=renderPlot({
    plot=df %>% 
      filter(Age>input$age[1], Age<input$age[2]) %>%
      filter(Country %in% input$typeInput) %>%
      ggplot(aes(x = Country)) + geom_bar(fill = "red")
    plot
  })
})

