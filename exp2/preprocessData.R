library(dplyr)

# load data
data <- read.csv('rawdata.csv')

# filter data according to pre-registered criteria
for(i in 1:max(data$nsubj)){
  subj <- data[data$nsubj==i,]
  
  # did the subject explore all decks?
  if(length(unique(subj$cardSelected)) < 4){
    cat('Subject', i, 'has been excluded due to not exploring all decks\n')
    data <- data[data$nsubj != i,]
  }
  
  # now check confidence levels
  subj %>%
    group_by(confidence) %>%
    mutate(conffreq = n()) %>%
    ungroup() -> subj 
  
  subjprop <- .85 * length(subj$trial)
  if(any(subj$conffreq>subjprop)){
    data <- data[data$nsubj != i,]  # same rating >85% time, exclude subj 
    cat('Subject', i, 'has been excluded due to same confidence in more than 85 trials\n')
  }
  
}


# compute whether choice has been correct, given order of decks
data$correct <- NA
for(i in unique(data$nsubj)){
  o <- data[data$nsubj==i,]$deckOrder[1]
  o <- toupper(o)
  
  data[data$nsubj==i,]$correct <- ifelse(data[data$nsubj==i,]$cardSelected==substr(o,3,3)
                                         | data[data$nsubj==i,]$cardSelected==substr(o,4,4), 
                                         1, 0)
  
}


# add block column
data %>%
  mutate(block = case_when(
    trial <= 20 ~ 1, 
    trial > 20 & trial <= 40 ~ 2, 
    trial > 40 & trial <= 60  ~ 3, 
    trial > 60 & trial <= 80  ~ 4, 
    trial > 80 ~ 5
  )) -> data


# compute metacognition & p(correct) per block
source('auroc2.R')

data$metacog <- NA; data$perfBlock <- NA
for(i in unique(data$nsubj)){
  for(j in 1:5){
    # block correct rate
    data[data$nsubj==i & data$block==j,]$perfBlock <- mean(data[data$nsubj==i & data$block==j,]$correct)
    
    # metacog
    data[data$nsubj==i & data$block==j,]$metacog <- type2roc(data[data$nsubj==i & data$block==j,]$correct,
                                                             data[data$nsubj==i & data$block==j,]$confidence,
                                                             4)
  }
}

# compute p(correct) per block 


write.csv(data, 'filtereddata.csv',row.names=F)
