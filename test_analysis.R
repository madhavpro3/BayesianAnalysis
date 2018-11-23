library(tidyverse)
#library(zoo)

setwd("C:\\Users\\admin\\Documents\\Madwork\\other\\IITM2018_Placement")

question_points<-read.csv("test_point.csv");
question_points<-question_points %>%
  mutate(QuestionNum=1:n());

vec_CorrectAns<-as.character(question_points$CorrectAns)
vec_QuesPoints<-as.numeric(question_points$Points)

user_responses_main<-read.csv("user_responses.csv")

user_responses<-user_responses_main[,21:40]
user_responses<-apply(user_responses,2,as.character)

num_users<-nrow(user_responses)
num_questions<-ncol(user_responses)

user_isCorrect<-matrix(0,nrow=num_users,ncol=num_questions)
for(rowInx in 1:num_users){
  user_isCorrect[rowInx,]=as.numeric(user_responses[rowInx,]==vec_CorrectAns)
}

## checking if inputting is done the right way
user_score<-numeric(num_users)
for(rowInx in 1:num_users){
  user_score[rowInx]=sum(user_isCorrect[rowInx,]*vec_QuesPoints);
}
which(user_score!=user_responses_main[,5])

# Assumptions
  # All candidates are similar in capability to answer

# Questions to ask

  # What is the posterior distribution of toughness of a question
  # What is the pmf of answering easy, medium and hard question by a candidate
    # Score the candidate based on the pmf and rank the candidates

# Advanced, this needs the order in which questions appeared for the candidate
  # Build the bayes net for nodes : Answered correctly, answered wrongly/not attempted
  # Build the bayes net for nodes : answered easy correctly, answered medium correctly, answered hard correctly
    # From this we can say if the random ordering of questions is favouring anyone 

# ConjugatePrior for Binomial distribution is beta distribution(Dirichlet of dimension 2)
# Conjugate prior for multinomial distribution is Dirichlet distribution
# ref : https://www.johndcook.com/CompendiumOfConjugatePriors.pdf
# ref : https://www.johndcook.com/blog/conjugate_prior_diagram/
# ref : https://en.wikipedia.org/wiki/Conjugate_prior
# ref : https://tminka.github.io/papers/minka-multinomial.pdf

# If a prior distribution gives a posterior in the same 'FAMILY OF DISTRIBUTION', then the prior and posterior
# are conjugate distribution and the prior is conjugateprior to the posterior.
# So if you know want certain prior distribution, pick its conjugate prior

# --------------------------------------------

# MADNOTE : Before going to the multinomial, try the binomial cases of 
# easy vs not-easy
# medium vs not-medium
# hard vs not-hard

# ref : http://www.obscureanalytics.com/2012/07/04/to-the-basics-bayesian-inference-on-a-binomial-proportion/
# What is the posterior distribution of toughness of a question
# Prior for 1pt question
# p1_easy is a beta-distribution with mean=0.6
p1_easy_mean=0.6; # mean probability of 1pt question being easy = 0.6


# Prior for 2pt question
p2_medium=0.6; # probability of 2pt question being easy = 0.6
p2_notmedium=1-p2_medium;

# Prior for 3pt question
p3_hard=0.6; # probability of 3pt question being easy = 0.6
p3_nothard=1-p3_hard;

easy_questionIndex<-which(vec_QuesPoints==1);
medium_questionIndex<-which(vec_QuesPoints==2);
hard_questionIndex<-which(vec_QuesPoints==3);


i_priorToughness<-c(p1_easy,p1_medium,p1_hard)
num_solvedCorrectly=sum(user_isCorrect[,2]);
num_notSolvedCorrectly=num_users-num_solvedCorrectly;

p_easy<-i_priorToughness[0];
p_medium<-i_priorToughness[1];
p_hard<-i_priorToughness[2];

# add choose so that number large numbers multiplied with small numbers
v1=choose(num_users,num_solvedCorrectly)*(p_easy^num_solvedCorrectly)*((1-p_easy)^num_notSolvedCorrectly);
v2=(p_medium^num_solvedCorrectly)*((1-p_medium)^num_notSolvedCorrectly);
v3=(p_hard^num_solvedCorrectly)*((1-p_hard)^num_notSolvedCorrectly);
sum=v1+v2+v3;
print(paste(v1," ",v2," ",v3));

a<-question_posteriorToughness(2,c(0.6,0.2,0.2))






