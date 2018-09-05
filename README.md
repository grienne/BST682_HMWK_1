# BST682_HMWK_1
# 
# ---
  
  title: "Homework 1 - BST682 - Answers"
output:
  pdf_document:
  toc: yes
toc_depth: '3'
html_document:
  highlight: tango
theme: paper
toc: yes
toc_depth: 3
word_document:
  toc: yes
toc_depth: '3'

---




##BST 682 Homework 1
##In order to be comfortable using R exclusively, will endeavor to do all work in R
##I will also annotate my notes so that months down the line I know what I did and why. 

##Question 1
##Uber, AirBnb and Stata have 3000, 1500, and 800 employees, respectively, and 30, 45, and 65 percent of
##these employees respectively are women. Resignations are equally likely among companies and genders. One
##woman resigns. What is the probability she worked for Uber?
  
uber <- 3000
airbnb <- 1500
stata <- 800
women <- (uber * .30) + (airbnb*.45) + (stata * .65)
(uber *30) / women
##Answer: 42.96% chance that the women resigning will be from UBER


##Question 2
##You flip four fair coins. Assuming the flips are independent, what is the pmf for the number of tails flipped?

##Terminology: p.m.f - probability mass function - function that defines probability of the discrete random variable X 
## taking on a particular value (in this cases tails, out of heads/tails). When you take all possible values (sample space)
##and associated probabilities into consideration, this is a discrete probability distribution as defined by a pmf.

tails.distr.df <- data_frame(value = c(0,1,2,3,4), prob= c(1/16, 1/4, 3/4, 1/4, 1/16))

##The values are the possible coin clip outcomes, 0 tails, 1, 2, 3, and 4 tails.)
##The prob are the probability of getting (or not getting a tails)

tails.distr.df %>% ggplot(aes(x=value, y=prob)) + geom_bar(stat = "identity") + ylim(c(0,1)) + xlab("X - Number of Tails") + ylab("f(X) Probability")
##the %>% is a piping nomenclature (reference Evernote - PhD - BST 682 - Homework 1)
##Plotting Notes - Reference Text  - Discovering Statistics Using R


##Question 3. Exercise 1.6a.; 1.6b
##Reference: https://stackoverflow.com/questions/26734744/how-to-plot-the-log-likelihood-of-binomial-distribution 

F <- c(18,31,34,33,27,33,28,23,33,12,19,25,14,4,22,7)
M <- c(11,22,27,29,24,29,25,26,38,14,23,31,20,6,34,12)
Y <- F
N <- F + M

#We input the table into the varied variables

##1.6a
Y / N
##This gives the proportion per progeny group

#Answer
## [1] 0.6206897 0.5849057 0.5573770 0.5322581 0.5294118 0.5322581 0.5283019 0.4693878 0.4647887 0.4615385 0.4523810
##[12] 0.4464286 0.4117647 0.4000000 0.3928571 0.3684211

##1.6b
##Maximum Likelihood - find the model/parameter that mostly generated the data.
##Typically one takes the Log formula which turns it into an addition problem, but in R see below

logL <- function(p) sum(log(dbinom(Y, N, p)))
##Reference: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Binomial.html 
optimize(logL, lower=0, upper=1, maximum=TRUE)

#Answer: 0.4945611, Objective - -39.31
##Reference: https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/optimize 


##Question 4. Assume annual rainfall in Lexington is normally distributed with a mean of 40 inches and standard deviation
##of 4. What is the probability that it takes more than 7 years before having a rainfall over 55 inches? What
##assumptions are you making?

year1 <- pnorm(55, mean = 40, sd=4, lower.tail = FALSE)  
##Note: Lower.tail = false because we want to see higher than 55

year1 ^ 7 
##Answer 4.224353e-29
##Probability of it taking more than 7 years before having rainfall over 55 inches. 
##We are assuming the mean/sd remain constant. We are assuming these values are independent (they most certainly are not)


### Problem 5: linear models refresher

##Using the data from `Table 2.3 Birthweight and gestational age.xls`
##calculate by matrix algebra the effect estimate resulting from regressing birth weight on gestational age.
##Note. The excel file had spaces instead of underscores, changed row titles in excel before importing. 

  
Table_2_3_Birthweight_and_gestational_age$xy <- (Table_2_3_Birthweight_and_gestational_age[,2] * Table_2_3_Birthweight_and_gestational_age[,1])

Table_2_3_Birthweight_and_gestational_age$x_squared <- (Table_2_3_Birthweight_and_gestational_age[,1]^2)  

Table_2_3_Birthweight_and_gestational_age$y_squared <- (Table_2_3_Birthweight_and_gestational_age[,2]^2)  


sum_gage <- colSums(Table_2_3_Birthweight_and_gestational_age[,1])

sum_bwt <- colSums(Table_2_3_Birthweight_and_gestational_age[,2])

sum_xy <- colSums(Table_2_3_Birthweight_and_gestational_age[,4])

sum_x2 <- colSums(Table_2_3_Birthweight_and_gestational_age[,5])

sum_y2 <- colSums(Table_2_3_Birthweight_and_gestational_age[,6])

colSums(Table_2_3_Birthweight_and_gestational_age[,6])

##Here we add in the columns required, x*y, x^2, y^2
##Sum the values of the new columns for calculation and turn all items into vectors for later use (see below)        
        
##Intercept Calculations
intercept_gage_bwt <- (((sum_bwt*sum_x2) - (sum_gage * sum_xy))) / ((((24 * sum_x2) - (sum_gage^2))))
intercept_gage_bwt
##Answer. -1484.985
##Slope Calculations
slope_gage_bwt <- ((24*sum_xy) - (sum_gage*sum_bwt)) / (((24*sum_x2) - (sum_gage^2)))
slope_gage_bwt
#Answer 115.5283
#
##Question 6 You will inevitably use the Google to problem solve with programming in R – many of you already do. Having
##go to resources for answering your questions and/or developing new skills can be quite helpful. Search around
##for what might be (or already is) a resource you will turn to as you improve your R skills. Give the site and
##url. What, in particular, makes this suitable for you?


##Answer. I use a book called Discovering Statistics using R. Online I use https://math.stackexchange.com
## for the statistics questions, and https://stackoverflow.com for R programming questions
##Mostly I use them because they are fairly easy to search through and ask a variety of questions with exhaustive responses. 


##Question 7 Plot birthweight by age and give each gender a diﬀerent color on the same plot. 
##(Tip: look at the Introduction to R notes). What observations do you have?

q7p1 <- ggplot(Table_2_3_Birthweight_and_gestational_age, aes(gestational_age, birth_weight, colour = sex))
q7p1 + geom_point()

##Now, do the same plot stratiﬁed by gender 
xyplot(birth_weight ~ gestational_age|sex, data = Table_2_3_Birthweight_and_gestational_age)
##requires (lattice package, stratifying n sex)
##Answer. Females have more weight diversity, makes have a sharper increase in weight by age than females. 


##Question 8. Using R and lm, conﬁrm your regression parameter estimate in Problem 5.
q8lm <- lm(formula = birth_weight ~ gestational_age, data= Table_2_3_Birthweight_and_gestational_age)
summary(q8lm)
##Answer Intercept. -1485.0, gestational_age. 115.5