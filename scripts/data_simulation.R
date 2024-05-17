# Load packages
require(dplyr)
require(data.table)

# Define task parameters --------------------------------------------------

n_pt        <- 30  # Number of participants
n_trials    <- 200 # Number of trials
amt_reward  <- 1   # Reward amount on each trial
amt_penalty <- 0   # Penalty amount on each trial

pr_reversal        <- .10                    # Probability of reversal on each new trial
pr_win_good_option <- .7                     # Probability of reward if chose good option
pr_win_bad_option  <- 1 - pr_win_good_option # Probability of reward if chose bad option

min_trials_since_reversal <- 5   # We artificially set the minimum trials before a reversal can occur
max_trials_since_reversal <- 25  # And the maximum number of trials since reversal that is allowable

# Generate trials -----------------------------------------------------------

## Create container for all trials for all participants
subject     <- rep(1:n_pt, each = n_trials)    # Participant id  
trial_no    <- rep(1:n_trials, times = n_pt)   # Trial number
cresp       <- rep(0, n_pt * n_trials)         # Good option on trial t (cresp = correct response)
reward      <- rep(0, n_pt * n_trials)         # Actual feedback on trial t (reward or penalty amount)
trials_since_reversal <- rep(0, n_pt*n_trials) # Number of trials since last reversal occurred
reversal_event  <- rep(0, n_pt*n_trials)       # Binary indicator of whether a reversal has occurred on the current trial
win_if_choose_a <- rep(0, n_pt*n_trials)       # Amount to be rewarded if chosen a
win_if_choose_b <- rep(0, n_pt*n_trials)       # Amount to be rewarded if chosen b      


## Bind all the above into one data table
sim_data <- data.table(subject, trial_no, cresp, reward, trials_since_reversal, reversal_event)

for (subject_x in 1:n_pt) {
  # Print visual confirmation
  message(paste("Generating trial contingencies for Subject ID ", subject_x))
  for (trial_t in 1:n_trials){
    if (trial_t == 1){
      # If trial is the first one, pick the good option at chance level
      good_option <- sample(0:1, prob = (c(0.5, 0.5)), size = 1)
      
      # and write the good option to sim_data
      sim_data[subject == subject_x & trial_no == trial_t, "cresp"] <- good_option
    } else {
      
      ## If trial is not the first one, 
      ## we update the counter from the last trial
      n_trials_since_reversal <- sim_data[subject==subject_x & trial_no == trial_t-1, trials_since_reversal] + 1
      
      ## we sample for whether a reversal should happen
      ## But we only do it if it has been >5 trials AND <= 25 trials since reversals 
      if (n_trials_since_reversal <= min_trials_since_reversal){
        reversal <- 0
      } else if (n_trials_since_reversal > max_trials_since_reversal){
        reversal <- 1
      } else {
        reversal <- sample(0:1, prob = c(1-pr_reversal, pr_reversal), size = 1)
      }

      ## If a reversal should occur on the current trial, we reset the number of trials since reversal to 0
      if (reversal == 1){
        n_trials_since_reversal <- 0
      } 
      
      ## We grab the good option from the previous trial
      good_option      <- sim_data[subject == subject_x & trial_no == trial_t - 1, cresp]
      
      ## Create a temporary container of available options - in our case, only 2. 
      possible_options <- 0:1
     
      ## And we swap out the good option if a reversal should occur
      good_option <- ifelse(reversal == 1, sample(possible_options[!possible_options %in% good_option], size = 1), good_option)
        
      ## Write all relevant data to sim_data
      sim_data[subject == subject_x & trial_no == trial_t, "cresp"]                 <- good_option
      sim_data[subject == subject_x & trial_no == trial_t, "trials_since_reversal"] <- n_trials_since_reversal
      sim_data[subject == subject_x & trial_no == trial_t, "reversal_event"]        <- reversal
      
    }
    
    ## Next, we need to determine, according to the good option, whether a participant gets rewarded or not
    ## Remember that it is not certain they will be rewarded, even if they chose the good option
    if (good_option == 0){
      reward_a <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_good_option, pr_win_bad_option))
      reward_b <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_bad_option, pr_win_good_option))
    } else{
      reward_a <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_bad_option, pr_win_good_option))
      reward_b <- sample(c(amt_reward, amt_penalty), size = 1, prob = c(pr_win_good_option, pr_win_bad_option))
    }
    
    ## Write the possible rewards to sim_data
    sim_data[subject == subject_x & trial_no == trial_t, "win_if_choose_a"] <- reward_a
    sim_data[subject == subject_x & trial_no == trial_t, "win_if_choose_b"] <- reward_b
  }
}


# Simulate participant choices --------------------------------------------

set.seed(123)

## Sample participant parameters
eta_list  <- pnorm(rnorm(n_pt, mean = 0, sd = 1))
beta_list <- pnorm(rnorm(n_pt, mean = 0, sd = 1)) * 20


## Write true parameter values to sim_data
for (subject_x in unique(sim_data$subject)){
  sim_data[subject == subject_x, "true_eta"]  <- eta_list[subject_x]
  sim_data[subject == subject_x, "true_beta"] <- beta_list[subject_x]
}

## Create columns to hold choices, Q values and probability values
sim_data[, ptresp := as.double()]        # Participant choice
sim_data[, pt_QA := as.double()]         # Participant's subjective Q value associated with stimulus A
sim_data[, pt_QB := as.double()]         # Participant's subjective Q value associated with stimulus B
sim_data[, pr_choose_a := as.double()]   # Participant's probability of choosing A (after applying softmax function)
sim_data[, outcome:=as.double()]         # Outcome of participant's choice

for (subject_x in unique(sim_data$subject)){
  
  message(paste("Simulating choices for Subject ID:", subject_x))
  
  for (trial_t in 1:n_trials){
    if (trial_t == 1){
      ## If it is trial 1, participant will randomly pick either stimulus at chance level
      pr_choose_a <- 1/2 
      pt_choice   <- sample(0:1, size = 1, prob = c(.5, .5))
      
      ## Their Q values for trial #1 is equivalent to the summed weighted possible outcomes, with each outcome weighted by the probability of being the good/bad choice
      QA <-  amt_reward * .5 + amt_penalty * .5
      QB <-  amt_reward * .5 + amt_penalty * .5
      sim_data[subject == subject_x & trial_no == trial_t, "pt_QA"] <- QA
      sim_data[subject == subject_x & trial_no == trial_t, "pt_QB"] <- QB
      
    } else{
      
      ## If not trial 1, then use Q values to determine probability of picking A
      QA <- sim_data[subject == subject_x & trial_no == trial_t, pt_QA]
      QB <- sim_data[subject == subject_x & trial_no == trial_t, pt_QB] 
      
      beta <- unique(sim_data[subject == subject_x, "true_beta"]) %>% as.numeric()
      # pr_choose_a <- exp(beta*QA)/(exp(beta*QA) + exp(beta*QB))
      pr_choose_a <- 1/(1 + exp(-beta*(QA-QB)))
      
      ## Then sample participant choice according to pr_choose_a
      pt_choice <- sample(0:1, size = 1, prob = c(pr_choose_a, 1 - pr_choose_a))
      
    }
    
    ## Next, we determine the outcome of the participant's choice. 
    ## We already have the rewards associated with each stimulus written into sim_data, so we can pick out the outcome that corresponds with their choice
    outcome <- ifelse(pt_choice == 0, sim_data[subject == subject_x & trial_no == trial_t, win_if_choose_a], sim_data[subject == subject_x & trial_no == trial_t, win_if_choose_b])
    
    ## Then, we update Q values
    eta <- unique(sim_data[subject == subject_x, "true_eta"])
    
    if (pt_choice == 0){
      QA_next_trial <- QA + eta*(outcome - QA)
      QB_next_trial <- QB
    } else {
      QA_next_trial <- QA
      QB_next_trial <- QB + eta*(outcome - QB)
    }
    
    ## Write the above information into sim_data
    sim_data[subject == subject_x & trial_no == trial_t, "pr_choose_a"] <- pr_choose_a
    sim_data[subject == subject_x & trial_no == trial_t, "ptresp"]      <- pt_choice
    sim_data[subject == subject_x & trial_no == trial_t, "outcome"]     <- outcome
    
    ## We also record the Q values for the next trial, but only if the current trial t isn't the last trial
    ## Else, there's no point in recording Q values for the next trial 
    if (trial_t != n_trials){
      sim_data[subject == subject_x & trial_no == trial_t + 1, 'pt_QA'] <- QA_next_trial
      sim_data[subject == subject_x & trial_no == trial_t + 1, 'pt_QB'] <- QB_next_trial
    }
  }
}


# Export simulated data to csv --------------------------------------------------

write.csv(sim_data, file = here::here("data", "simulated_data_1.csv"))




