library(knitr)

library(knitr)
knit_hooks$set(small.mar = function(before, options, envir) {
  if (before) par(mar = c(5.1, 4.1, 2.1, 2.1))  # smaller margin on top 
})
opts_chunk$set(fig.path='figure/beamer-',fig.align='center',fig.show='hold',size='footnotesize', small.mar=TRUE)


require(dplyr)

survey_data <- read.csv("/home/prantoran/datasets/survey_results_public.csv")

## testing whether there is any correlation between considering coding as a hobby and open-source contribution

## H0 = there is no difference 
## between the mean number of people who opensource and consider coding as a hobby
## and the mean number of people who opensource and does not consider coding as a hobby

hobby <- subset(survey_data, Hobby == 'Yes')
hobbyNo <- subset(survey_data, Hobby == 'No')

## subset(hobbyNo, Respondent == 'NA')

## hobby$Hobby

## Converting OpenSource = 'Yes' to 1 and OpenSource = 'No' to 0
hobby <- hobby %>%
  mutate(OpenSource = ifelse(OpenSource == 'No', 0, 1))

hobby$OpenSource

hobbyNo <- hobbyNo %>%
  mutate(OpenSource = ifelse(OpenSource == 'No', 0, 1))

hobbyNo$OpenSource

htest = t.test(hobby$OpenSource, hobbyNo$OpenSource)

# data:  hobby$OpenSource and hobbyNo$OpenSource
# t = 67.561, df = 32428, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2343996 0.2484065
# sample estimates:
#   mean of x mean of y 
# 0.4821458 0.2407427   

# htest$p.value


## Testing whether the difference between mean salaries is zero

# selecting only two columns
prunedConvSalaryData = survey_data %>% select(Gender, ConvertedSalary)
# is.na(prunedSalaryData$ConvertedSalary)

# removing NA values
prunedConvSalaryData <- na.omit(prunedConvSalaryData)
prunedConvSalaryData

fivenum(survey_data$ConvertedSalary, na.rm = TRUE)

femaleSalaries <- subset(prunedConvSalaryData, Gender == 'Female')
femaleSalaries
maleSalaries <- subset(prunedConvSalaryData, Gender == 'Male')
maleSalaries

htest2 = t.test(femaleSalaries$ConvertedSalary, maleSalaries$ConvertedSalary)
htest2


