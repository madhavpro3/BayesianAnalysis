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
# p1_easy is a beta-distribution with mean=0.6, concentration=50(=a+b)
# a=mean*concentration, b = concentration-a
p1_easy_mean=0.6; # mean probability of 1pt question being easy = 0.6
concentration_easy=50;
a_easy=p1_easy_mean*concentration_easy;b_easy=concentration_easy-a_easy;

# The prior distribution
# the probability of solving this question correctly is a RV aroung p1_easy_mean
plot(density(rbeta(10000,a_easy,b_easy)));

num_solvedCorrectly=sum(user_isCorrect[,2]);
num_notSolvedCorrectly=num_users-num_solvedCorrectly;

a_easy_posterior=a_easy+num_solvedCorrectly;
b_easy_posterior=b_easy+num_notSolvedCorrectly;
plot(density(rbeta(10000,a_easy_posterior,b_easy_posterior)));

posterior_shapevals<-function(mean,concentration,Y,N){
  # Y = Responders ; N = sampleSize
  # mean=a/a+b; concentration=a+b; refer wikipedia
  a_prior=mean*concentration;b_prior=concentration-a_prior;
  
  a_posterior=a_prior+Y;
  b_posterior=b_prior+N-Y;
  return(c(a_posterior,b_posterior));
}

question_posterior<-function(quesNum,mean_prior){
  Y=sum(user_isCorrect[,quesNum]);
  return(posterior_shapevals(mean_prior,50,Y,num_users));
}


plot(density(rbeta(1000,30,20)),col="green",cex=5);
par(new=TRUE);
plot(density(rbeta(1000,35,15)));


df<-gather(data.frame(priori=rbeta(1000,30,20),posteriori=rbeta(1000,35,15)))
gg<-ggplot(df);
gg+geom_density(aes(x=value,color=key),size=2)+
  xlim(0,1)+theme_bw()+
  xlab("Probability of answering correctly")+ylab("")
  

user_index=1;
easy_questionIndex<-which(vec_QuesPoints==1);
medium_questionIndex<-which(vec_QuesPoints==2);
hard_questionIndex<-which(vec_QuesPoints==3);

num_solvedCorrectly=sum(user_isCorrect[user_index,easy_questionIndex])