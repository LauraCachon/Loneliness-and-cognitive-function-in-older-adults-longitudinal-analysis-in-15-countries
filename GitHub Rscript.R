## LONELINESS AND COGNITIVE FUNCTION IN OLDER ADULTS: LONGITUDINAL ANALYSIS IN 15 COUNTRIES- Supplemental material ##

#This file is a supplement to the paper "Loneliness and cognitive function in older adults: longitudinal analysis in 15 countries" by Cachon-Alonso et al. (2023). It contains the code used to conduct the main analysis included in the paper in the following order:
#1. Modelling steps
#2. Multiple group RICLPM
#3. RICLPM adjusted for confounders
#Our analysis were conducted with lavaan (Rosseel, 2012) following the coding steps proposed by Mulder and Hamaker (2021) [link] https://jeroendmulder.github.io/RI-CLPM/lavaan.html

#Data comes from waves 5 to 8 of the Survey of Health, Ageing and Retirement in Europe (SHARE). In our study, we included two measures of loneliness, four measures of cognitive function, and data on seven confounders. The final dataset included 56,049 adults (3 repeated measures). 
#Here, we show as an example the code for running the models that explore the associations between verbal fluency and the three-item measure of loneliness. All the other models followed the same structure.

## 1.Modelling steps: constrained vs. grand means contrained; unconstrained vs. constrained model ##

#Example dataset:
lon3.flu <- final.wide %>% select(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                  y1 = fluency_5, y2 = fluency_6, y3 = fluency_7) %>%
  select(matches('[xy][1-3]'))

#Define the unconstrained RICLM:
unconstrained<- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3
  RIy =~ 1*y1 + 1*y2 + 1*y3 
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 

  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
 
 # Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2

 # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3

  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
'

#Apply the unconstrained model to the data
unconst.lon3flu<- lavaan(unconstrained, data = lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(unconst.lon3flu, standardized = T, fit.measures = T)


#Define the RICLPM with only grand means constrained:
onlymeansconstrained <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3
  RIy =~ 1*y1 + 1*y2 + 1*y3 
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 

  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
 
# Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2

 # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3

  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
 
 # Constrain the grand means over time. 
x1 + x2 + x3  ~ mx*1
y1 + y2 + y3  ~ my*1

'
#Apply the RICLPM with only means constrained to our data, and compare its fit with the RICLPM unconstrained 
means.lon3flu<- lavaan(onlymeansconstrained, data = lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(means.lon3flu, standardized = T, fit.measures = T)
anova(unconst.lon3flu, means.lon3flu)

#Define the RICLPM with constrained grand means and lagged coefficients:
constrained<- '# Create between components (random intercepts)
RIx =~ 1*x1 + 1*x2 + 1*x3 
RIy =~ 1*y1 + 1*y2 + 1*y3 

# Create within-person centered variables
wx1 =~ 1*x1
wx2 =~ 1*x2
wx3 =~ 1*x3 
wy1 =~ 1*y1
wy2 =~ 1*y2
wy3 =~ 1*y3


# Estimate the lagged effects between the within-person centered variables (constrained).
wx2 ~ a*wx1 + b*wy1 
wy2 ~ c*wx1 + d*wy1
wx3 ~ a*wx2 + b*wy2
wy3 ~ c*wx2 + d*wy2


# Estimate the covariances between the residuals of the within-person centered variables 
# (the innovations, constrained).
wx2 ~~ cov*wy2
wx3 ~~ cov*wy3


# Estimate the covariance between the within-person centered variables at the first wave. 
wx1 ~~ wy1 # Covariance

# Estimate the variance and covariance of the random intercepts. 
RIx ~~ RIx
RIy ~~ RIy
RIx ~~ RIy

# Estimate the (residual) variance of the within-person centered variables (constrained).
wx1 ~~ wx1 # Variance
wy1 ~~ wy1 
wx2 ~~ vx*wx2 # Residual variance
wy2 ~~ vy*wy2 
wx3 ~~ vx*wx3 
wy3 ~~ vy*wy3 

# Constrain the grand means over time. 
x1 + x2 + x3  ~ mx*1
y1 + y2 + y3  ~ my*1
'
#Apply the RICLPM fully constrained and compare its fit with the RICLPM unconstrained 
# loneliness3+ fluency
const.lon3flu <- lavaan(constrained, data = lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(const.lon3flu, standardized = T, fit.measures = T)
anova(const.lon3flu, uncont.lon3flu)



## 2. Multiple group RICLPM (Extension 2/constrained, Mulder and Hamaker, 2021) ##
#New variable for year of birth before/after 1949 in 2013 (SHARE-wave5)
final.wide<- mutate(final.wide, G = yrbirth)
wideG<- final.wide %>% mutate(G=replace(G, G<1949, 1))
wide.G<- wideG %>% mutate(G=replace(G, G>1948, 0))

RICLPM.group <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3 
  RIy =~ 1*y1 + 1*y2 + 1*y3 
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
 
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  
  
  # Estimate the lagged effects between the within-person centered variables. Constrain the  
  # autoregressive effects across groups. 
  wx2 ~ c(a1, a1)*wx1 + c(b1, b1)*wy1
  wy2 ~ c(c1, c1)*wx1 + c(d1, d1)*wy1
  wx3 ~ c(a2, a2)*wx2 + c(b2, b2)*wy2
  wy3 ~ c(c2, c2)*wx2 + c(d2, d2)*wy2
  
  
  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx2 ~~ wy2
  wx3 ~~ wy3
 
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 

'
#Example of dataset for the multigorup version of the RILCPM 
#lonely3+fluency
lon3.fluG <- wide.G %>% rename(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                               y1 = fluency_5, y2 = fluency_6, y3 = fluency_7, G=G) %>%
  select(matches('[xy][1-3]'), G)


#Applying the multigroup version and comparing it with the one-group model
RICLPM.G.lon3flu <- lavaan(RICLPM.group, data = lon3.fluG, group = "G", meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR")
summary(RICLPM.G.lon3flu)
anova(const.lon3flu, RICLPM.G.lon3flu)



#The multigroup version had a better fit, so we applied the RICLPM constrained to both age groups separately.
#We created two databases stratifying by age, and applied the constrained RICLPM to each of them (2 age groups x 2 loneliness measures x 4 cognitive function measures).

wide_new.young<-wide_new %>% filter(yrbirth>1948) 
wide_new.old<-wide_new %>% filter(yrbirth<1949)

#loneliness3+ fluency (younger group)
lon3.y.flu <- wide_new.young %>% select(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                        y1 = fluency_5, y2 = fluency_6, y3 = fluency_7) %>%
  select(matches('[xy][1-3]'))

RICLPM5.y.lon3flu <- lavaan(constrained, data = lon3.y.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(RICLPM5.y.lon3flu, standardized = T, fit.measures = T, ci=T)

#loneliness3+ fluency (older group)
lon3.o.flu <- wide_new.old %>% select (x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                       y1 = fluency_5, y2 = fluency_6, y3 = fluency_7) %>%
  select(matches('[xy][1-3]'))

RICLPM5.o.lon3flu <- lavaan(constrained, data = lon3.o.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(RICLPM5.o.lon3flu, standardized = T, fit.measures = T, ci=T)



## 3.RICLPM adjusted for confounders ##

#Grouping countries by geographical area and creating dummy variables 
x<-wide_new %>% mutate(country = recode(country, Austria="W", Germany="W", Sweden="N", Netherlands= "W", Denmark="N", France="W", Switzerland="W", Belgium="W", Luxembourg="W", Spain="S", Italy="S", "Czech Republic"="E", Slovenia="E", Estonia="E"))
x<-filter(x, country == "W" | country == "N" | country == "S" | country == "E" | country == "Israel")

library(fastDummies)
x<-dummy_cols(x, select_columns = "country")

z10wide_new.young<-x %>% filter(yrbirth>1948)
z10wide_new.old<-x %>% filter(yrbirth<1949)

#RICLPM adjusted for confounders (included as time-invariant predictors of the random intercepts. Extension 1/ Mulder and Hamaker, 2021)
RICLPM.z10<-
  '# Create between components (random intercepts)
RIx =~ 1*x1 + 1*x2 + 1*x3 
RIy =~ 1*y1 + 1*y2 + 1*y3 

# Create within-person centered variables
wx1 =~ 1*x1
wx2 =~ 1*x2
wx3 =~ 1*x3 
wy1 =~ 1*y1
wy2 =~ 1*y2
wy3 =~ 1*y3

# Regression of random intercepts on z1. 
RIx + RIy ~ z1 # Constrained over time. 
RIx + RIy ~ z2
RIx + RIy ~ z3
RIx + RIy ~ z4
RIx + RIy ~ z5
RIx + RIy ~ z6
RIx + RIy ~ z7
RIx + RIy ~ z8
RIx + RIy ~ z9
RIx + RIy ~z10

  # Estimate the lagged effects between the within-person centered variables (constrained).
wx2 ~ a*wx1 + b*wy1 
wy2 ~ c*wx1 + d*wy1
wx3 ~ a*wx2 + b*wy2
wy3 ~ c*wx2 + d*wy2

# Estimate the covariances between the residuals of the within-person centered variables 
# (the innovations, constrained).
wx2 ~~ cov*wy2
wx3 ~~ cov*wy3

# Estimate the covariance between the within-person centered variables at the first wave. 
wx1 ~~ wy1 # Covariance

# Estimate the variance and covariance of the random intercepts. 
RIx ~~ RIx
RIy ~~ RIy
RIx ~~ RIy

# Estimate the (residual) variance of the within-person centered variables (constrained).
wx1 ~~ wx1 # Variance
wy1 ~~ wy1 
wx2 ~~ vx*wx2 # Residual variance
wy2 ~~ vy*wy2 
wx3 ~~ vx*wx3 
wy3 ~~ vy*wy3 

# Constrain the grand means over time. 
x1 + x2 + x3  ~ mx*1
y1 + y2 + y3  ~ my*1
'


#Dataset: loneliness3+ fluency (young group as example)
z10.y.lon3.flu <- z10wide_new.young %>% select(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                               y1 = fluency_5, y2 = fluency_6, y3 = fluency_7, z1 = education_5, z2= sex, z3= age_5, z4= disease_sum_5, z5=depsum_5, z6=partner_5, z7=country_W, z8=country_N, z9=country_E,z10=country_S)

#Full Information Maximum Likelihood (main analysis)
z10.y.lon3flu<- lavaan(RICLPM.z10, data = z10.y.lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") #Method: MLR
summary(z10.y.lon3flu, standardized = T, fit.measures = T, ci=T)

#Multiple Imputation (complementary analysis)
set.seed(3)
z10.y.lon3flu_mi <- runMI(RICLPM.z10, data=z10.y.lon3.flu, m=5, miPackage = "mice", fun="lavaan", meanstructure=T) 
summary(z10.y.lon3flu_mi, standardized = T, fit.measures = T, ci=T)




