# Discrimination and wages example from
#   Causal Inference: The Mixtape
#   by Scott Cunningham

browseURL("https://mixtape.scunning.com/03-directed_acyclical_graphs")

library(tidyverse)   # Data etc
library(ggdag)       # Directed acyclic graphs in ggplot
library(stargazer)   # Create tables

library(ivreg)       # IV estimator

######################
### A model of a dgp #
######################

# Coordinates on screen for a DAG - set these by trial and error
cdag <- list(x=c(Female=0.33, Discrimination=0.7, Wage=1,   Ability=1, Occupation=0.4),
             y=c(Female=0,    Discrimination=0,   Wage=0.5, Ability=1, Occupation=0.66))

# Relations etc. Three equations, labels, coords
dag1 <- dagify(Wage           ~ Discrimination + Occupation + Ability,
               Occupation     ~ Ability + Discrimination, 
               Discrimination ~ Female, 
               exposure = "Discrimination", 
               latent   = "Ability",
               outcome  = "Wage",
               coords   = cdag
               ) %>% 
  tidy_dagitty() %>%                 # Turn into tidy dataframe
  node_collider()                    # Detect colliders and label them

dag1

# Plot using canned routine ggdag
dag1 %>%                                  
  ggdag(text_col="red", stylized=TRUE) + 
  theme_dag_grey()

# Plot using geoms - gives more control
dag1 %>% 
  ggplot(aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_dag_node(color="grey11", shape=21, fill="orange", alpha=0.5) + 
  geom_dag_edges_arc(curvature=0.15) + 
  geom_dag_text(color="red", size=7.5) +
  theme_dag() 

# Highlight colliders using canned ggdag_collider (need to remove previously detected ones)
dag1 %>% 
  select(-colliders) %>% 
  ggdag_collider(text_col="darkgreen", stylized=TRUE) 

# Using geoms
dag1 %>% 
  ggplot(aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_dag_node(aes(colour=colliders)) + 
  geom_dag_edges() + 
  geom_dag_text(color="darkgreen") +
  theme_dag()

# Uisng geoms - nicer, specifying arrow type etc
a1 <- grid::arrow(angle=25, length=unit(0.125,"inches"), ends="last", type="open")
dag1 %>% 
  ggplot(aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_dag_node(aes(colour=colliders), shape=21, fill="grey88", size=12.5) + 
  geom_dag_edges_arc(curvature=0.15, 
                     edge_color="grey77", edge_width=1.25,
                     arrow=a1) + 
  geom_dag_text(color="darkgreen", size=5) +
  theme_dag()

###########################
### A numerical example ###
###########################

n  <- 10000   # No. of observations (a lot)

tb <- tibble(female         = ifelse(runif(n)>=0.5, 1, 0),
             ability        = rnorm(n),
             discrimination = female,
             occupation     = 1 - 0.3*discrimination + 1.2*ability + rnorm(n),
             wage           = 1 - 0.2*discrimination + 1.5*ability + 1*occupation + rnorm(n)
             )

# Three regressions
#1. The wage on the discrimination variable ;
#2. The wage on discrimination and occupation;
#3. The wage on discrimination, occupation and _the unavailable_ ability variable!

lm_1  <- lm(wage  ~ female, data=tb)
lm_2  <- lm(wage  ~ female + occupation, data=tb)
lm_3  <- lm(wage  ~ female + occupation + ability, data=tb)

stargazer(lm_1, lm_2, lm_3, type = "text", style = "qje",
          # keep.stat=c("n","adj.rsq"),
          column.labels = c("Biased Unconditional", 
                            "Biased",
                            "Unbiased Conditional"))

#########################################
### An aside on removing the collider ###
#########################################

dag0  <- dagify(Wage           ~ Discrimination + Occupation + Ability,
                Occupation     ~ Discrimination, 
                Discrimination ~ Female, 
                exposure = "Discrimination", 
                latent   = "Ability",
                outcome  = "Wage",
                coords   = cdag) %>% 
  tidy_dagitty() %>%
  node_collider()

dag0 %>%  
  ggplot(aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_dag_node(aes(colour=colliders)) + 
  geom_dag_edges() + 
  geom_dag_text(color="darkgreen") +
  theme_dag() 

# Simulate data
tb0 <- tb %>% 
  mutate(
    occupation0  = 1 - 0.3*discrimination + rnorm(n),
    wage0        = 1 - 0.2*discrimination + 1.5*ability + 1*occupation0 + rnorm(n)
  )

lm_1a <- lm(wage0 ~ female, data=tb0)
lm_2a <- lm(wage0 ~ female + occupation0, data=tb0)
lm_3a <- lm(wage0 ~ female + occupation0 + ability, data=tb0)

stargazer(lm_1a, lm_2a, lm_3a, type = "text", 
          keep.stat=c("n","adj.rsq"),
          column.labels = c("Unconditional", 
                            "Potential bias (no Ability)",
                            "Unbiased"))

#################
## IV solution ##
#################

# Create a new variable as an IV

cdag2  <- list(x=c(Female=0.33, Discrimination=0.7, Wage=1,   Ability=1, Occupation=0.4, Distance=0.33),
               y=c(Female=0,    Discrimination=0,   Wage=0.5, Ability=1, Occupation=0.5, Distance=1))

dag2  <- dagify(Wage           ~ Discrimination + Occupation + Ability,
                Occupation     ~ Ability + Discrimination, 
                Discrimination ~ Female, 
                Occupation     ~ Distance,
                exposure = "Occupation", 
                latent   = "Ability",
                outcome  = "Wage",
                coords   = cdag2) %>% 
  tidy_dagitty() %>% 
  node_collider() %>% 
  node_instrumental(outcome="Wage", exposure="Occupation") %>% 
  mutate(instrumental = if_else(is.na(instrumental), "invalid", instrumental))

# Plot collider graph
dag2 %>% 
  select(-colliders) %>%                                # Take colliders out so it can recalculate
  ggdag_collider(text_col="darkgreen", stylized=TRUE) 

# Plot instrument graph
dag2 %>% 
  ggdag_instrumental(text_col="darkgreen", stylized=TRUE) 

# Nicer versions of both
dag2 %>% 
  ggplot(aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_dag_node(aes(colour=colliders), shape=21, fill="grey88", size=12.5) + 
  geom_dag_edges_arc(curvature=0.15, 
                     edge_color="grey77", edge_width=1.25,
                     arrow=a1) + 
  geom_dag_text(color="darkgreen", size=5) +
  theme_dag()

dag2 %>% 
  ggplot(aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_dag_node(aes(colour=instrumental), shape=21, fill="grey88", size=12.5) + 
  geom_dag_edges_arc(curvature=0.15, 
                     edge_color="grey77", edge_width=1.25,
                     arrow=a1) + 
  geom_dag_text(color="darkgreen", size=5) +
  theme_dag()

#################################
#### IV Example #################
#################################

tb <- tibble( female         = ifelse(runif(n)>=0.5, 1, 0),
              ability        = rnorm(n),
              discrimination = female,
              dist           = runif(n, 0, 5),
              occupation     = 1 - 0.3*discrimination + 1.2*ability - 0.1*dist + rnorm(n),
              wage           = 1 - 0.2*discrimination + 1.5*ability + 1*occupation + rnorm(n)
              )

lm_2a <- lm(wage ~ female + occupation, data=tb)
lm_iv <- ivreg(wage ~ female + occupation | female + dist, data=tb)

stargazer(lm_2a, lm_iv, type="text", style="qje", column.labels=c("Biased","Unbiased"))
