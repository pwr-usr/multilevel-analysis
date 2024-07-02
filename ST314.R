# ST314 Individual Project
# Candidate Number: 24155
# Question 1
###############################################################################
library(ggplot2)
library(dplyr)
library(lme4)
library(sjPlot)
library(sjstats)
library(leaps)
library(ggvis)
library(xtable)
library(lmtest)
library(MuMIn)
library(forcats)
set.seed(24155)
pval_for_lmer_summary <- function(model) {
  coef_table <- summary(model)$coefficients
  p_values <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
  # round all values to four digits
  p_values <- round(p_values, 6)
  coef_table <- round(coef_table, 4)
  return(cbind(coef_table, p_values))
}
chisq_p <- function(mrs, mri) {
  lr <- 2*(logLik(mrs)[1]-logLik(mri)[1])
  0.5*(1-pchisq(lr,1)) + 0.5*(1-pchisq(lr,2))
}
pisadata <- read.csv("pisaUK.csv")
pisadata <- subset(pisadata,select=-c(age,hedres))
head(pisadata, 2)
# response variable zread
############################
# Converting covriate types
# Covariates types and names for pisadata
# str(pisadata)
# Convert female to factor
pisadata$female <- as.factor(pisadata$female)
# immig to factor
pisadata$immig <- as.factor(pisadata$immig)
# schltype to factor
pisadata$schltype <- as.factor(pisadata$schltype)
# how mnay schools
pisadata$schoolid <- as.factor(pisadata$schoolid)
length(unique(pisadata$schoolid))
# Check the data types
str(pisadata)
# Convert categorical variables to factors with labeled levels
pisadata$female <- factor(pisadata$female, levels = c("0", "1"), labels = c("Male", "Female"))
pisadata$immig <- factor(pisadata$immig, levels = c("1", "2", "3"), labels = c("Native", "2nd gen", "1st gen"))
pisadata$schltype <- factor(pisadata$schltype, levels = c("1", "2", "3"), labels = c("Private independent", "Private government-dependent", "Public"))
# Summarise pisadata
summary(pisadata)
# summary only for numerical variables
summary(pisadata[, sapply(pisadata, is.numeric)])
# SD of all numerical variables
sapply(pisadata[, sapply(pisadata, is.numeric)], sd)

# An exploration of between- and within-cluster variation in the response variable


#######################
# Plots for pisa data
# Compute school-level means of zread
school_data <- pisadata %>%
  group_by(schoolid) %>%
  summarise(mean_reading_score = mean(zread),
            school_type = first(schltype),
            student_teacher_ratio = first(stratio),
            school_size = first(schsize))

# Plot school-level predictors against school-level means of zread
ggplot(school_data, aes(x = school_type, y = mean_reading_score, fill = school_type)) +
  geom_boxplot(show.legend = FALSE) +
  xlab("School Type") +
  ylab("School-level Mean of Reading Score") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

ggplot(school_data, aes(x = student_teacher_ratio, y = mean_reading_score)) +
  geom_point(color = "#4D4D4D") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Student-Teacher Ratio") +
  ylab("School-level Mean of Reading Score") +
  theme_minimal()

ggplot(school_data, aes(x = school_size, y = mean_reading_score)) +
  geom_point(color = "#66A61E") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("School Size") +
  ylab("School-level Mean of Reading Score") +
  theme_minimal()

# Plot individual-level predictors against zread
ggplot(pisadata, aes(x = female, y = zread, fill = female)) +
  geom_boxplot(show.legend = FALSE) +
  xlab("Gender") +
  ylab("Reading Score") +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4")) +
  theme_minimal()

ggplot(pisadata, aes(x = immig, y = zread, fill = immig)) +
  geom_boxplot(show.legend = FALSE) +
  xlab("Immigration Status") +
  ylab("Reading Score") +
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#FB9A99")) +
  theme_minimal()

ggplot(pisadata, aes(x = hisced, y = zread)) +
  geom_point(color = "#984EA3") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  xlab("Highest Education of Parents") +
  ylab("Reading Score") +
  theme_minimal()

ggplot(pisadata, aes(x = wealth, y = zread)) +
  geom_hex(bins = 20) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  xlab("Family Wealth") +
  ylab("Reading Score") +
  theme_minimal()

ggplot(pisadata, aes(x = cultposs, y = zread)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Cultural Possessions at Home") +
  ylab("Reading Score") +
  theme_minimal()

ggplot(pisadata, aes(x = lmins, y = zread)) +
  geom_point(color = "#80B1D3") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  xlab("Minutes Learning per Week") +
  ylab("Reading Score") +
  theme_minimal()

# Plot the distribution of zread across schools
ggplot(pisadata, aes(x = zread)) +
  geom_density(fill = "#A6CEE3", color = "#1F78B4") +
  xlab("Reading Score") +
  ylab("Density") +
  theme_minimal()


# Variance Components Model
single_level_model <- lm(zread ~ 1, data = pisadata)

variance_component_model <- lmer(zread ~ 1 + (1 | schoolid), data = pisadata)
summary(variance_component_model)
performance::icc(variance_component_model)
anova(variance_component_model, single_level_model)
#vc_confint <- confint(variance_component_model)
print(vc_confint, digits = 3)
# standardize all numerical predictor except zread which is the outcome
stdpisa <- pisadata %>% 
  mutate_at(c("hisced", "wealth", "cultposs", "lmins", "stratio", "schsize"), ~(scale(.) %>% as.vector))

# Begin of model selection
########################
# six level 1 predictors to choose from
full_model <- lmer(zread ~ female + immig + hisced + wealth + cultposs + lmins + (1 | schoolid),
                   data = stdpisa, REML = FALSE, na.action = "na.fail")

# Generate all possible subsets of fixed effects
subsets <- dredge(full_model, rank = "AIC", fixed = ~ (1 | schoolid))

# View the results
# All six selected
print(subsets)

# Get the best model 
m2 <- get.models(subsets, subset = 1)[[1]]

# Print the summary of the best model
summary(m2)
# Manually compute p values
pval_for_lmer_summary(m2)
performance::icc(m2)
# conf_m2 <- confint(m2)
print(conf_m2, digits = 3)
# immig2nd gen not significant, but keep the whole variable

################################################################################
# Test interactions
# female:cultposs
m3_cultposs <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                      female:cultposs + (1 | schoolid), data = stdpisa, REML = FALSE)
# female:hisced
m3_hisced <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                    female:hisced + (1 | schoolid), data = stdpisa, REML = FALSE)
# female:wealth
m3_wealth <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                    female:wealth + (1 | schoolid), data = stdpisa, REML = FALSE)
# female:lmins
m3_lmins <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                   female:lmins + (1 | schoolid), data = stdpisa, REML = FALSE)
# female:immig
m3_immig <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                   female:immig + (1 | schoolid), data = stdpisa, REML = FALSE)
anova(m2,m3_cultposs)
anova(m2,m3_hisced)
anova(m2,m3_wealth)
anova(m2,m3_lmins)
anova(m2, m3_immig)
# No significant interaction with gender


# IMPORTANT: How much of the level 1 and 2 variances do the level
# 1 covariates (and any interactions) explain?


# Define the base model formula
base_formula <- zread ~ cultposs + female + hisced + immig + lmins + wealth + (1 | schoolid)

# Define the level 1 variables
level1_vars <- c("cultposs", "female", "hisced", "immig", "lmins", "wealth")

# Create an empty data frame to store the interaction results
interaction_df <- data.frame(Interaction = character(), Coefficient = numeric(), P_Value = numeric())

# Iterate through all possible two-way interactions
for (i in 1:(length(level1_vars) - 1)) {
  for (j in (i + 1):length(level1_vars)) {
    var1 <- level1_vars[i]
    var2 <- level1_vars[j]
    
    # Create the interaction formula
    interaction_formula <- as.formula(paste0("zread ~ ", var1, " * ", var2, " + (1 | schoolid)"))
    
    # Fit the lmer model with the interaction
    model_interaction <- lmer(interaction_formula, data = stdpisa)
    
    # Extract the interaction terms, coefficients, and p-values
    interaction_summary <- pval_for_lmer_summary(model_interaction)
    interaction_terms <- rownames(interaction_summary)[grep(":", rownames(interaction_summary))]
    
    # Iterate through each interaction term
    for (term in interaction_terms) {
      coefficient <- interaction_summary[term, "Estimate"]
      p_value <- interaction_summary[term, "p_values"]
      
      # Add the interaction result to the data frame
      interaction_df <- rbind(interaction_df, data.frame(Interaction = term, Coefficient = coefficient, P_Value = p_value))
    }
  }
}

# Format the interaction terms for better readability
interaction_df$Interaction <- gsub(":", " × ", interaction_df$Interaction)
interaction_df$Interaction <- gsub("Female", "(Female)", interaction_df$Interaction)
interaction_df$Interaction <- gsub("2nd gen", "(2nd gen)", interaction_df$Interaction)
interaction_df$Interaction <- gsub("1st gen", "(1st gen)", interaction_df$Interaction)

# sort interaction_df by P_Value
interaction_df <- interaction_df[order(interaction_df$P_Value), ]
interaction_df
# cultposs × wealth -0.0503  0.0000
# cultposs × hisced      0.0402  0.0002
# immig(1st gen) × lmins     -0.1308  0.0017
# cultposs × lmins     -0.0225  0.0142

# Select cultposs × wealth first because most significatn
m_cultposs_wealth <- lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
                            wealth:cultposs + (1 | schoolid), data = stdpisa, REML = FALSE)
anova(m_cultposs_wealth, m2)
# add one more select from the best of two interactions models
m_cultposs_wealth_hiced <-  lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
                                   hisced:cultposs + wealth:cultposs + (1 | schoolid), data = stdpisa, REML = FALSE)
# significant
anova(m_cultposs_wealth_hiced,m_cultposs_wealth, m2)
pval_for_lmer_summary(m_cultposs_wealth_hiced)
# three interactions
m_three_interaction <- lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
                              hisced:cultposs + wealth:cultposs + immig:lmins + (1 | schoolid), data = stdpisa, REML = FALSE)
pval_for_lmer_summary(m_three_interaction)
# 2nd gen not significant, but first gen immigrant study more but lower marks is very intersting
# Worsened BIC, so not included, current best is m_cultposs_wealth_hiced at this stage
# interpret the following output
anova(m_three_interaction, m_cultposs_wealth_hiced, m_cultposs_wealth, m2)
summary(m_cultposs_wealth_hiced)
# conf_bestlevel1 <- confint(m_cultposs_wealth_hiced)
print(conf_bestlevel1, digits = 3)
pval_for_lmer_summary(m_cultposs_wealth_hiced)
performance::icc(m_cultposs_wealth_hiced)
#########################
# Level 2 covariates starts from best model with level 1 covariates and its interactions
bestlevel1int <- m_cultposs_wealth_hiced
# Level 2 covariates
# scholtype -0.0503  0.0000
# 
m7 <- lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
             hisced:cultposs + wealth:cultposs + 
             schltype + (1 | schoolid), data = stdpisa, REML = FALSE)
anova(m7, bestlevel1int)

pval_for_lmer_summary(m7)
# stratio, significatn but closer to boundary p 0.02901
m8 <- lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
             hisced:cultposs + wealth:cultposs + 
             stratio + (1 | schoolid), data = stdpisa, REML = FALSE)
pval_for_lmer_summary(m8)
anova(m8, bestlevel1int)
# schsize, p 0.036
m9 <- lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
             hisced:cultposs + wealth:cultposs + 
             schsize + (1 | schoolid), data = stdpisa, REML = FALSE)
pval_for_lmer_summary(m9)
anova(m9, bestlevel1int)
# Select scholtype first
# scholtype and stratio comparing with m7: not significant
# not adding stratio
m10 <- lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
              hisced:cultposs + wealth:cultposs + 
              stratio + schltype + (1 | schoolid), data = stdpisa, REML = FALSE)
pval_for_lmer_summary(m10)
anova(m10, m7)
# scholtype and schsize significant
m11 <- lmer(zread ~ cultposs + female + hisced + wealth +immig + lmins + 
              hisced:cultposs + wealth:cultposs + 
              schsize + schltype + (1 | schoolid), data = stdpisa, REML = FALSE)
pval_for_lmer_summary(m11)
anova(m11, m7)

# Conclusion: delete stratio, keep scholtype and schsize
# Best model so far is m11
summary(m11)
# m11_conf <- confint(m11)
m11_conf
pval_for_lmer_summary(m11)
# important: explain why icc decreased
performance::icc(m11)
# Random slope model


# Fit the random slope models manually
m_cultposs <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                     hisced:cultposs + wealth:cultposs + 
                     schsize + schltype + (1 + cultposs | schoolid), data = stdpisa, REML = FALSE)

m_female <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                   hisced:cultposs + wealth:cultposs + 
                   schsize + schltype + (1 + female | schoolid), data = stdpisa, REML = FALSE)

m_hisced <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                   hisced:cultposs + wealth:cultposs + 
                   schsize + schltype + (1 + hisced | schoolid), data = stdpisa, REML = FALSE)

m_wealth <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                   hisced:cultposs + wealth:cultposs + 
                   schsize + schltype + (1 + wealth | schoolid), data = stdpisa, REML = FALSE)
isSingular(m_wealth)
m_immig <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                  wealth:cultposs + hisced:cultposs +
                  schsize + schltype + (1 + immig | schoolid), data = stdpisa, REML = FALSE,
                control = lmerControl(optimizer ="Nelder_Mead"))
isSingular(m_immig)
m_lmins <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                  hisced:cultposs + wealth:cultposs + 
                  schsize + schltype + (1 + lmins | schoolid), data = stdpisa, REML = FALSE,
                control = lmerControl(optimizer ="Nelder_Mead"))

# Calculate p-values using the chisq_p function
chisq_p <- function(mrs, mri) {
  lr <- 2*(logLik(mrs)[1]-logLik(mri)[1])
  0.5*(1-pchisq(lr,1)) + 0.5*(1-pchisq(lr,2))
}

p_cultposs <- chisq_p(m_cultposs, m11)
p_female <- chisq_p(m_female, m11)
p_hisced <- chisq_p(m_hisced, m11)
p_wealth <- chisq_p(m_wealth, m11)
p_immig <- chisq_p(m_immig, m11)
p_lmins <- chisq_p(m_lmins, m11)
# crate a dataframe with p value results
results <- data.frame(
  random_slope = c("cultposs","female","hisced","wealth","immig","lmins"),
  p_value = c(p_cultposs, p_female, p_hisced, p_wealth, p_immig, p_lmins)
)
print(results, digits = 2)
# Female immig lmins are significant
# female 0.046819977 variance femaleFemale 0.01351
# immig 0.002328923
# lmins 0.040150635

VarCorr(m_immig)
VarCorr(m_female)
VarCorr(m_lmins)

summary(m_immig)
summary(m_female)
summary(m_lmins)

(aic_values <- c(AIC(m_immig), AIC(m_female), AIC(m_lmins)))
(bic_values <- c(BIC(m_immig), BIC(m_female), BIC(m_lmins)))

# Choose m_immig because higher variance than female and m_lmins
# choose female because more relevant to research question
m_female
# try add one more random slope

m_female_cultposs <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                            hisced:cultposs + wealth:cultposs + 
                            schsize + schltype + (1 + cultposs + female | schoolid),
                          data = stdpisa, REML = FALSE)

m_female_hisced <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                          hisced:cultposs + wealth:cultposs + 
                          schsize + schltype + (1 + hisced + female | schoolid),
                        data = stdpisa, REML = FALSE)

m_female_wealth <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                          hisced:cultposs + wealth:cultposs + 
                          schsize + schltype + (1 + wealth + female | schoolid),
                        data = stdpisa, REML = FALSE)
isSingular(m_female_wealth)

# Failed convergence, ignore
m_female_immig <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                         wealth:cultposs + hisced:cultposs +
                         schsize + schltype + (1 + immig + female | schoolid),
                       data = stdpisa, REML = FALSE,
                       control = lmerControl(optimizer = "Nelder_Mead"))


m_female_lmins <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                         hisced:cultposs + wealth:cultposs + 
                         schsize + schltype + (1 + lmins + female | schoolid),
                       data = stdpisa, REML = FALSE,
                       control = lmerControl(optimizer = "Nelder_Mead"))
m_female_lmins <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                         hisced:cultposs + wealth:cultposs + 
                         schsize + schltype + (1 + lmins + female | schoolid),
                       data = stdpisa, REML = FALSE)
anova(m_female_cultposs, m_female)
anova(m_female_hisced, m_female)
anova(m_female_wealth, m_female)
anova(m_female_lmins, m_female) # only significant one

summary(m_female_lmins)
pval_for_lmer_summary(m_female_lmins)
VarCorr(m_female_lmins)
# conf_female_lmins <- confint(m_female_lmins)
print(conf_female_lmins, digits = 3)
performance::icc(m_female_lmins)
###############################################################################
# Cross level interactions
m_base <- m_female_lmins
m_cultposs_schsize <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                             hisced:cultposs + wealth:cultposs + 
                             schsize + schltype + cultposs:schsize + (1 + lmins + female | schoolid),
                           data = stdpisa, REML = FALSE)

m_cultposs_schltype <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                              hisced:cultposs + wealth:cultposs + 
                              schsize + schltype + cultposs:schltype + (1 + lmins + female | schoolid),
                            data = stdpisa, REML = FALSE)
# Convergence issue
m_female_schsize <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                           hisced:cultposs + wealth:cultposs + 
                           schsize + schltype + female:schsize + (1 + lmins + female | schoolid),
                         data = stdpisa, REML = FALSE,
                         control = lmerControl(optimizer = "Nelder_Mead"))

# Singular
m_female_schltype <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                            hisced:cultposs + wealth:cultposs + 
                            schsize + schltype + female:schltype + (1 + lmins + female | schoolid),
                          data = stdpisa, REML = FALSE,
                          control = lmerControl(optimizer = "Nelder_Mead"))

m_hisced_schsize <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                           hisced:cultposs + wealth:cultposs + 
                           schsize + schltype + hisced:schsize + (1 + lmins + female | schoolid),
                         data = stdpisa, REML = FALSE)

m_hisced_schltype <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                            hisced:cultposs + wealth:cultposs + 
                            schsize + schltype + hisced:schltype + (1 + lmins + female | schoolid),
                          data = stdpisa, REML = FALSE)

m_wealth_schsize <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                           hisced:cultposs + wealth:cultposs + 
                           schsize + schltype + wealth:schsize + (1 + lmins + female | schoolid),
                         data = stdpisa, REML = FALSE)

m_wealth_schltype <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                            hisced:cultposs + wealth:cultposs + 
                            schsize + schltype + wealth:schltype + (1 + lmins + female | schoolid),
                          data = stdpisa, REML = FALSE)

m_immig_schsize <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                          hisced:cultposs + wealth:cultposs + 
                          schsize + schltype + immig:schsize + (1 + lmins + female | schoolid),
                        data = stdpisa, REML = FALSE,
                        control = lmerControl(optimizer = "Nelder_Mead"))

m_immig_schltype <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                           hisced:cultposs + wealth:cultposs + 
                           schsize + schltype + immig:schltype + (1 + lmins + female | schoolid),
                         data = stdpisa, REML = FALSE)

m_lmins_schsize <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                          hisced:cultposs + wealth:cultposs + 
                          schsize + schltype + lmins:schsize + (1 + lmins + female | schoolid),
                        data = stdpisa, REML = FALSE)

m_lmins_schltype <- lmer(zread ~ cultposs + female + hisced + wealth + immig + lmins + 
                           hisced:cultposs + wealth:cultposs + 
                           schsize + schltype + lmins:schltype + (1 + lmins + female | schoolid),
                         data = stdpisa, REML = FALSE)
results_df <- data.frame(
  Interaction = c("cultposs:schsize", "cultposs:schltype", "female:schsize", "female:schltype",
                  "hisced:schsize", "hisced:schltype", "wealth:schsize", "wealth:schltype",
                  "immig:schsize", "immig:schltype", "lmins:schsize", "lmins:schltype"),
  AIC = c(AIC(m_cultposs_schsize), AIC(m_cultposs_schltype), AIC(m_female_schsize), AIC(m_female_schltype),
          AIC(m_hisced_schsize), AIC(m_hisced_schltype), AIC(m_wealth_schsize), AIC(m_wealth_schltype),
          AIC(m_immig_schsize), AIC(m_immig_schltype), AIC(m_lmins_schsize), AIC(m_lmins_schltype)),
  BIC = c(BIC(m_cultposs_schsize), BIC(m_cultposs_schltype), BIC(m_female_schsize), BIC(m_female_schltype),
          BIC(m_hisced_schsize), BIC(m_hisced_schltype), BIC(m_wealth_schsize), BIC(m_wealth_schltype),
          BIC(m_immig_schsize), BIC(m_immig_schltype), BIC(m_lmins_schsize), BIC(m_lmins_schltype)),
  LRT_p_value = c(anova(m_base, m_cultposs_schsize)$`Pr(>Chisq)`[2], 
                  anova(m_base, m_cultposs_schltype)$`Pr(>Chisq)`[2],
                  anova(m_base, m_female_schsize)$`Pr(>Chisq)`[2], 
                  anova(m_base, m_female_schltype)$`Pr(>Chisq)`[2],
                  anova(m_base, m_hisced_schsize)$`Pr(>Chisq)`[2], 
                  anova(m_base, m_hisced_schltype)$`Pr(>Chisq)`[2],
                  anova(m_base, m_wealth_schsize)$`Pr(>Chisq)`[2], 
                  anova(m_base, m_wealth_schltype)$`Pr(>Chisq)`[2],
                  anova(m_base, m_immig_schsize)$`Pr(>Chisq)`[2], 
                  anova(m_base, m_immig_schltype)$`Pr(>Chisq)`[2],
                  anova(m_base, m_lmins_schsize)$`Pr(>Chisq)`[2], 
                  anova(m_base, m_lmins_schltype)$`Pr(>Chisq)`[2]),
  Slope_Variance = c(VarCorr(m_cultposs_schsize)$schoolid[2, 2], 
                     VarCorr(m_cultposs_schltype)$schoolid[2, 2],
                     VarCorr(m_female_schsize)$schoolid[2, 2],
                     VarCorr(m_female_schltype)$schoolid[2, 2],
                     VarCorr(m_hisced_schsize)$schoolid[2, 2],
                     VarCorr(m_hisced_schltype)$schoolid[2, 2],
                     VarCorr(m_wealth_schsize)$schoolid[2, 2],
                     VarCorr(m_wealth_schltype)$schoolid[2, 2],
                     VarCorr(m_immig_schsize)$schoolid[2, 2],
                     VarCorr(m_immig_schltype)$schoolid[2, 2],
                     VarCorr(m_lmins_schsize)$schoolid[2, 2],
                     VarCorr(m_lmins_schltype)$schoolid[2, 2]),
  RE_Variance_Slope = c(attr(VarCorr(m_cultposs_schsize)$schoolid, "stddev")[2],
                        attr(VarCorr(m_cultposs_schltype)$schoolid, "stddev")[2],
                        attr(VarCorr(m_female_schsize)$schoolid, "stddev")[2],
                        attr(VarCorr(m_female_schltype)$schoolid, "stddev")[2],
                        attr(VarCorr(m_hisced_schsize)$schoolid, "stddev")[2],
                        attr(VarCorr(m_hisced_schltype)$schoolid, "stddev")[2],
                        attr(VarCorr(m_wealth_schsize)$schoolid, "stddev")[2],
                        attr(VarCorr(m_wealth_schltype)$schoolid, "stddev")[2],
                        attr(VarCorr(m_immig_schsize)$schoolid, "stddev")[2],
                        attr(VarCorr(m_immig_schltype)$schoolid, "stddev")[2],
                        attr(VarCorr(m_lmins_schsize)$schoolid, "stddev")[2],
                        attr(VarCorr(m_lmins_schltype)$schoolid, "stddev")[2]),
  RE_Variance_Intercept = c(attr(VarCorr(m_cultposs_schsize)$schoolid, "stddev")[1],
                            attr(VarCorr(m_cultposs_schltype)$schoolid, "stddev")[1],
                            attr(VarCorr(m_female_schsize)$schoolid, "stddev")[1],
                            attr(VarCorr(m_female_schltype)$schoolid, "stddev")[1],
                            attr(VarCorr(m_hisced_schsize)$schoolid, "stddev")[1],
                            attr(VarCorr(m_hisced_schltype)$schoolid, "stddev")[1],
                            attr(VarCorr(m_wealth_schsize)$schoolid, "stddev")[1],
                            attr(VarCorr(m_wealth_schltype)$schoolid, "stddev")[1],
                            attr(VarCorr(m_immig_schsize)$schoolid, "stddev")[1],
                            attr(VarCorr(m_immig_schltype)$schoolid, "stddev")[1],
                            attr(VarCorr(m_lmins_schsize)$schoolid, "stddev")[1],
                            attr(VarCorr(m_lmins_schltype)$schoolid, "stddev")[1])
)
results_df <- results_df[order(results_df$LRT_p_value), c("Interaction", "AIC", "BIC", "LRT_p_value")]
print(results_df, digits = 3)
# sort by LRT_p_value
# m_lmins_schltype
# m_immig_schltype
# m_immig_schsize
# hisced:schltype p val close to boundary so ignore


performance::icc(m_female_lmins)
performance::icc(m_lmins_schltype)
performance::icc(m_immig_schltype)
performance::icc(m_immig_schsize)

# p val significant for public and lmins
pval_for_lmer_summary(m_lmins_schltype)
# p val not significant
pval_for_lmer_summary(m_immig_schltype)
# p significant for immig1st and schsize lmins and immig 2nd gen no longer significant
#
pval_for_lmer_summary(m_immig_schsize)


# final model m_lmins_schltype
fm <- m_lmins_schltype
summary(fm)
pval_for_lmer_summary(fm)
# confint_fm <- confint(fm)
print(round(confint_fm, 3))
# sigma.fm <- as.data.frame(VarCorr(fm), comp = "Variance")
print(sigma.fm, digits = 2)
sig2u0 <- sigma.fm$vcov[1]
sig2u0
# Extract estimate of slope variance
sig2u1 <- sigma.fm$vcov[2]
sig2u1

#Estimate of slope
beta <- as.matrix(fixef(fm))
beta1 <- beta[2, ]
# Lower limit of 95% range
beta1 - 1.96 * sqrt(sig2u1)
# Upper limit of 95% range
beta1 + 1.96 * sqrt(sig2u1)

sigu01 <- sigma.fm$vcov[3]
sigu01






































# Question 2
###############################################################################
library(ggplot2)
library(dplyr)
library(lme4)
library(lattice)
pval_for_lmer_summary <- function(model) {
  coef_table <- summary(model)$coefficients
  p_values <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
  # round all values to four digits
  p_values <- round(p_values, 6)
  coef_table <- round(coef_table, 4)
  return(cbind(coef_table, p_values))
}
chisq_p <- function(mrs, mri) {
  lr <- 2*(logLik(mrs)[1]-logLik(mri)[1])
  0.5*(1-pchisq(lr,1)) + 0.5*(1-pchisq(lr,2))
}
sfdata <- read.csv("SF12.csv")
head(sfdata, 1)
# add the time-invariant covariates first
# response variable zpcs
str(sfdata)
# Should wave 
# sfdata$wave <- as.factor(sfdata$wave)
sfdata$female <- factor(sfdata$female,
                        levels = c("1","0"),
                        labels = c("Female", "Male"))
sfdata$ethnicity <- factor(sfdata$ethnicity,
                           levels = c("1", "2", "3", "4"),
                           labels = c("White", "Asian", "Black", "Other"))
sfdata$highed <- factor(sfdata$highed,
                        levels = c("1", "0"),
                        labels = c("DegreeOrHigher", "LowerThanDegree"))
sfdata$partner <- factor(sfdata$partner,
                         levels = c("1","0"),
                         labels = c("Yes","No"))
sfdata$tenurehh <- factor(sfdata$tenurehh,
                          levels = c("1","2","3"),
                          labels = c("Owner","SocialRental","PrivateRental"))
str(sfdata)
summary(sfdata)
# mean number of observations per individual
sfdata %>%
  group_by(ind) %>%
  summarise(n = n()) %>%
  summarise(mean(n))
# how many individuals
sfdata %>%
  summarise(n = n_distinct(ind))
# female proportion for individuals
sfdata %>%
  distinct(ind, .keep_all = TRUE) %>%
  group_by(female) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))
# tenurehh proportion
sfdata$tenurehh %>%
  table() %>%
  prop.table()
# ethnicity proportions for individuals
sfdata %>%
  distinct(ind, .keep_all = TRUE) %>%
  group_by(ethnicity) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))
sfdata$highed %>% 
  table() %>%
  prop.table()
sfdata %>%
  group_by(wave) %>%
  summarise(mean.age = mean(age), median.age = median(age), min.age = min(age), max.age = max(age),
            var.age = var(age))
tmpdata <- subset(sfdata, select = -c(age, highed, partner, tenurehh, income))
sfdata.wide <- reshape(tmpdata, idvar = "ind", v.names = c("zpcs"), timevar = "wave",
                       direction = "wide")
# Sort columns
zpcs_cols <- grep("zpcs", names(sfdata.wide), value = TRUE)
zpcs_cols <- zpcs_cols[order(as.numeric(sub("zpcs\\.", "", zpcs_cols)))]
other_cols <- c("ind", "female", "ethnicity")
final_cols <- c("ind", zpcs_cols, setdiff(other_cols, "ind"))
sfdata.wide <- sfdata.wide[, final_cols]
head(sfdata.wide, 1)
# Correlation matrix
cor_matrix <- cor(sfdata.wide[, 2:12], use = "pairwise.complete.obs")
print(cor_matrix, digits = 2)
cor_matrix <- round(cor_matrix, 2)
write.csv(cor_matrix, "cor_matrix.csv")

# Bivariate associations
# Age
ggplot(sfdata, aes(x = age60, y = zpcs)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Age (centred at 60)", y = "Physical Functioning Score")

# Gender
sfdata %>%
  group_by(ind, female) %>%
  summarise(mean_zpcs = mean(zpcs)) %>%
  ggplot(aes(x = female, y = mean_zpcs)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Mean Physical Functioning Score")

# Ethnicity
sfdata %>%
  group_by(ind, ethnicity) %>%
  summarise(mean_zpcs = mean(zpcs)) %>%
  ggplot(aes(x = ethnicity, y = mean_zpcs)) +
  geom_boxplot() +
  labs(x = "Ethnicity", y = "Mean Physical Functioning Score")

# Education
ggplot(sfdata, aes(x = highed, y = zpcs)) +
  geom_boxplot() +
  labs(x = "Education Level", y = "Physical Functioning Score")

# Housing Tenure
ggplot(sfdata, aes(x = tenurehh, y = zpcs)) +
  geom_boxplot() +
  labs(x = "Housing Tenure", y = "Physical Functioning Score")

# Income
ggplot(sfdata, aes(x = income, y = zpcs)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Log Income", y = "Physical Functioning Score")

# Partner
ggplot(sfdata, aes(x = partner, y = zpcs)) +
  geom_boxplot() +
  labs(x = "Partner Status", y = "Physical Functioning Score")

# Variation in mean and SD of zpcs across individuals
sfdata %>%
  group_by(ind) %>%
  summarise(mean_zpcs = mean(zpcs), sd_zpcs = sd(zpcs)) %>%
  ggplot() +
  geom_density(aes(x = mean_zpcs), color = "blue", fill = "blue", alpha = 0.5) +
  geom_density(aes(x = sd_zpcs), color = "red", fill = "red", alpha = 0.5) +
  labs(x = "Value", y = "Density", title = "Mean (Blue) and SD (Red) of zpcs") +
  scale_color_manual(name = "Statistic", values = c("blue", "red"), labels = c("Mean", "SD")) +
  scale_fill_manual(name = "Statistic", values = c("blue", "red"), labels = c("Mean", "SD"))








# Add age as random slope
# default optimizer failed to converge
mrs0 <- lmer(zpcs ~ age60 + (1 + age60|ind), data = sfdata, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))


# more appropirate to use age for growth curve analysis
sfdata$age60 <- sfdata$age - 60
summary(sfdata$age60)
# Variance Component Model
mri0 <- lmer(zpcs ~ (1|ind), data = sfdata, REML = FALSE)
summary(mri0)
pval_for_lmer_summary(mri0)
performance::icc(mri0)
confint(mri0)
# Add age
mri1 <- lmer(zpcs ~ age60 + (1|ind), data = sfdata, REML = FALSE)
summary(mri1)
pval_for_lmer_summary(mri1)
performance::icc(mri1)
anova(mri1, mri0)

# Residuals
sfdata$resid <- residuals(mri1)
# Histogram of residuals
hist(sfdata$resid, breaks = 20, main = "Histogram of Residuals",
     xlab = "Residuals")

# Normal Q-Q plot of residuals
qqnorm(sfdata$resid)
qqline(sfdata$resid)

# Add age as random slope
# default optimizer failed to converge
mrs0 <- lmer(zpcs ~ age60 + (1 + age60|ind), data = sfdata, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
# Predicted values
sfdata$pred <- fitted(mrs0)
sfdata6 <- sfdata[sfdata$ind <= 20, ]
# Plot of predicted values vs age, grouped by individual
xyplot(pred ~ age60, data = sfdata6, groups = ind, type = c("p", "l"),
       xlab = "Age (centered at 60)", ylab = "Predicted Physical Functioning")
summary(mrs0)
performance::icc(mrs0)
anova(mrs0, mri1)
pval_for_lmer_summary(mrs0)
# conf_mrs0 = confint(mrs0)
print(conf_mrs0)
# Consider squared age term
mrs1 <- lmer(zpcs ~ age60 + I(age60^2) + (1 + age60|ind), data = sfdata, REML = FALSE,
             control = lmerControl(optimizer ="Nelder_Mead"))
anova(mrs1, mrs0)
summary(mrs1)
VarCorr(mrs1)
pval_for_lmer_summary(mrs1)
performance::icc(mrs1)
# conf_mrs1 <- confint(mrs1)
print(conf_mrs1)
sfdata$pred <- fitted(mrs1)
sfdata6 <- sfdata[sfdata$ind <= 15, ]
# Plot of predicted values vs age, grouped by individual
xyplot(pred ~ age60, data = sfdata6, group = ind, type = c("p", "l"),
       xlab = "Age (centered at 60)", ylab = "Predicted Physical Functioning")
# add the time-invariant covariates first, followed by the time-varying covariates.
# This allows you to establish the baseline effects of stable characteristics before examining 
# the influence of dynamic factors.
# Add two time-invariant covariates
# failed to converge so centering age60
sfdata$age60by10 <- sfdata$age60/10
mrs4 <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + (1 + age60by10|ind),
             data = sfdata, REML = FALSE)

mrs3 <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + (1 + age60by10|ind),
             data = sfdata, REML = FALSE)

mrs2 <- lmer(zpcs ~ age60by10 + I(age60by10^2) + ethnicity + (1 + age60by10|ind),
             data = sfdata, REML = FALSE)
# compare two TI predictors with female only and no TI predictors
anova(mrs2, mrs1)
anova(mrs3, mrs1)
anova(mrs4,mrs3,mrs1)
anova(mrs3,mrs2)
# mrs3 is the only candidate, AIC, BIC worse and p is 1!!!

# Conclusion for TI variables,consider gender or ethnicity
# Add time varying covariates
mrs5 <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + (1 + age60by10|ind),
             data = sfdata, REML = FALSE)
# Try all combinations of TV predictors
###############################################################################
model_null <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_highed <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_partner <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + partner + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_tenurehh <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + tenurehh + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_income <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_highed_partner <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + partner + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_highed_tenurehh <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + tenurehh + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_highed_income <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_partner_tenurehh <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + partner + tenurehh + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_partner_income <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + partner + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_tenurehh_income <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + tenurehh + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

model_highed_partner_tenurehh <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + partner + tenurehh + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_highed_partner_income <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + partner + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_highed_tenurehh_income <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + tenurehh + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_partner_tenurehh_income <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + partner + tenurehh + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE)

model_full <- lmer(zpcs ~ age60by10 + I(age60by10^2) + female + ethnicity + highed + partner + tenurehh + income + (1 + age60by10 | ind), data = sfdata, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))


lrt_results <- anova(model_null, model_highed, model_partner, model_tenurehh, model_income,
                     model_highed_partner, model_highed_tenurehh, model_highed_income,
                     model_partner_tenurehh, model_partner_income, model_tenurehh_income,
                     model_highed_partner_tenurehh, model_highed_partner_income,
                     model_highed_tenurehh_income, model_partner_tenurehh_income, model_full)
rownames(lrt_results)[which.min(lrt_results$BIC)]
# sort lrt_results by AIC
lrt_results[order(lrt_results$AIC),]
lrt_results[order(lrt_results$BIC),]
# select best aic from each complexity
# partner is not significant
anova(model_full, model_highed_tenurehh_income, model_highed_tenurehh, model_tenurehh, model_null)

# ethnicity other is not significant
pval_for_lmer_summary(model_highed_tenurehh_income)
##############################################################################
# final model
fm <- model_highed_tenurehh_income
summary(fm)
VarCorr(fm)
pval_for_lmer_summary(fm)
performance::icc(model_highed_tenurehh_income)
# (conf_highed_tenurehh_income <- confint(model_highed_tenurehh_income))
print(conf_highed_tenurehh_income)
# generate predicted values for the final model
sfdata$pred <- fitted(fm)
# Residuals
sfdata$resid <- residuals(fm)
# Histogram of residuals
hist(sfdata$resid, breaks = 20, main = "Histogram of Residuals",
     xlab = "Residuals")

# Normal Q-Q plot of residuals
qqnorm(sfdata$resid)
qqline(sfdata$resid)

sfdata6 <- sfdata[sfdata$ind <= 10, ]
# Plot of predicted values vs age, grouped by individual
xyplot(pred + zpcs~ age60, data = sfdata6, groups = ind, type = c("p", "l"),
       xlab = "Age (centered at 60)", ylab = "Predicted Physical Functioning")

write.csv(pval_for_lmer_summary(fm), "q2pval_for_lmer_summary.csv")
write.csv(conf_highed_tenurehh_income, "q2confint.csv")
write.csv(VarCorr(fm), "q2VarCorr.csv")
write.csv(summary(fm), "q2anova.csv")
###############################################################################
# control = lmerControl(optimizer ="Nelder_Mead")
