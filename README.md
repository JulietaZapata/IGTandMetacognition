# IGTandMetacognition
This repository contains the data, the scripts for the data analysis and the scripts for the experimental task of the study "_Metacognitive sensitivity on the Iowa Gambling Task reveals awareness as a necessary condition for advantageous performance_" (Zapata, Comay, Taricco, Barttfeld, Solovey, Saal & Ahumada; _submitted_). 

# Scripts
* _analysis.R_ replicates all analysis and figures reported in the study. 
* _auroc2.R_ computes the area under a "type 2" ROC curve. 
* _preprocessData.R_ performs exclusion of subjects based on the reported criteria. 
* _simAUROC.R_ replicates figure 2 from the manuscript. 

# Data organization 
Data from each experiment is on the respective experiment folder in a .csv format. Two archives per experiments are present, one for the raw unprocessed data and other with the filtered data (i.e., with the reported subjects excluded).

Rows are trials, and columns are organized as follows: 
* _gender_: "m" or "f" for males and females, respectively.
* _age_: in integers for subjects' age.
* _nsubj_: subject number.
* _trialNumber_: trial number for each subject (from 1 to 100).
* _cardSelected_: the card picked by the subject on each trial.
* _rtChoice_: reaction times for the subjects' choices.
* _win_: amount of money earned on each trial.
* _loss_: amount of money lost on each trial.
* _net_: net amount of money obtained on each trial (i.e., win - loss).
* _totalcash_: total amount of money that the subject has on each trial. 
* _confidence_: the confidence of the subject on her choice.
* _rtConf_: reaction times for the subjects' confidence report.
* _correct_: "1" or "0" for advantageous (decks C or D) or disadvantageous (decks A or B) choices.
* _block_: indicator for the current block (1 to 5). 
* _metacog_: area under ROC curve by blocks of 20 trials (5 blocks).
* _perfBlock_: proportion of correct choices by blocks of 20 trials (5 blocks).

For experiment 2 one extra column is present (_order_) which reflects the ordering of the decks outcomes. For example, 'cbda' means that deck A had the payoff schedule of deck C, deck B had the payoff of deck B, deck C had the payoff of deck D and finally deck D had the payoff of deck A. 

# Contact
For any doubts or suggestions you can send an email to julieta.m.zapata@gmail.com 

