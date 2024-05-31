# Load packages
require(cmdstanr)
require(dplyr)
require(data.table)
require(tictoc)


# Set cmdstan path according to machine used
system_cmdstan_path <- c("C:/Users/aarka/Documents/.cmdstan/cmdstan-2.32.0", 
                         "C:/Users/gymno/Documents/.cmdstan/cmdstan-2.32.0")
for (path in system_cmdstan_path){
  if (file.exists(path)){
    set_cmdstan_path(path)
  }
}


# Load data
sim_data <- read.csv(here::here("data/simulated_data_2.csv")) %>% as.data.table()

# Set run model flag
run_model_flag <- T

# Prep for stan modelling -----------------------------------------

## Get no. of unique participants 
n_participants <- sim_data$subject %>% unique() %>% length()
n_trials       <- sim_data$trial_no %>% unique() %>% length()
n_conditions   <- sim_data$condition %>% unique() %>% length()

## Re-index participants for stan modelling
for (ID in unique(sim_data$subject)){
  index <- which(unique(sim_data$subject)==ID)
  sim_data[subject == ID, stan_index:=seq(1, n_participants)[index]]
}

## Re-index condition for stan modelling
sim_data[, condition_index:=ifelse(condition == "pre", 0, 1)]

# Organise data for Stan
stan_data <- list(
  ## Metadata
  N         = n_participants,                           # Number of participants 
  T         = n_participants * n_trials * n_conditions, # Number of trials
  n_options = 2,
  
  ## Indices
  p_ix      = sim_data$stan_index,      # Index of participant id
  trial_no  = sim_data[, trial_no],     # Index of trial numbers   
  condition = sim_data$condition_index, # Index of condition
  
  ## Data
  actions           = sim_data[, ptresp] ,                # Participant response
  outcomes          = sim_data[, outcome],                # Outcome of action (0 = nonreward, 1  = reward) 
  time_since_reversal = sim_data[, trials_since_reversal] # No. of trials since last reversal
)


# Load model library ------------------------------------------------------

source(here::here("scripts", "model_library.R"))
model_stan_dir <- here::here('scripts', 'stan-files')


# M1: Q Learning with Partial Pooling -------------------------------------

model_to_fit   <- model_q_learning_pp_offset

## Remove stan model .exe file if already exists
if(file.exists(here::here('scripts', 'stan-files', paste(model_to_fit$stan_file_noext, '.exe', sep = ""))) == T){
  file.remove(here::here('scripts', 'stan-files', paste(model_to_fit$stan_file_noext, '.exe', sep = "")))
}

## Pre-compile model
compiled_model <- cmdstan_model(
  stan_file       = here::here('scripts', 'stan-files', model_to_fit$stan_file),
  force_recompile = T
)


## Create containers for participant-level estimates
m1_est_beta      <- rep(NA, times = n_participants) ## Inverse temperature
m1_est_eta       <- rep(NA, times = n_participants) ## learning rate 
m1_est_delta_eta <- rep(NA, times = n_participants) ## Offset in learning rate in 'post' condition

m1_beta_within_50ci       <- rep(NA, times = n_participants)  ## Inverse temperature
m1_beta_within_90ci       <- rep(NA, times = n_participants)  ## Inverse temperature
m1_eta_within_50ci        <- rep(NA, times = n_participants)  ## Learning rate
m1_eta_within_90ci        <- rep(NA, times = n_participants)  ## Learning rate
m1_delta_eta_within_50ci        <- rep(NA, times = n_participants)  ## Learning rate
m1_delta_eta_within_90ci        <- rep(NA, times = n_participants)  ## Learning rate


## Sampling
if(run_model_flag){
  tic()
  m1_fit <- compiled_model$sample(
    data            = stan_data,
    chains          = 4,
    parallel_chains = 4,
    refresh         = 100,
    iter_warmup     = 500,
    iter_sampling   = 1000,
    save_warmup     = FALSE
  )
  toc()
  
  ## Play audio cue
  beepr::beep("fanfare")
}

## Save data
if (run_model_flag){
  ## Print and/or save samples
  m1_fit$save_output_files(
    dir      = here::here("output"),
    basename = model_to_fit$model_name
  )
}
  

# Extract variables ---------------- 

output_files <-  m1_fit$output_files()

## extract log-likelihood matrix
m1_log_likelihood <- read_cmdstan_csv(
  files               = output_files,
  variables           = c("choice_log_lik"),
  sampler_diagnostics = NULL,
  format              = getOption("cmdstanr_draws_format", "draws_df")
)

## extract predicted choices
m1_choice_pred <- read_cmdstan_csv(
  files               = output_files,
  variables           = c("choice_pred"),
  sampler_diagnostics = NULL,
  format              = getOption("cmdstanr_draws_format", "draws_df")
)

## get WAIC for model
m1_ll_samples <- as.matrix(
  m1_log_likelihood$post_warmup_draws[, 1:m1_log_likelihood$metadata$stan_variable_sizes$choice_log_lik,]
)
m1_model_WAIC <- loo::waic(m1_ll_samples)


#####################################################
## Extract parameter samples (Group-level)
#####################################################

m1_group_par_samples_all <- read_cmdstan_csv(
  files = output_files,
  variables = model_to_fit$group_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)

## Get median group parameter estimates
m1_group_par_samples <- as.matrix(m1_group_par_samples_all$post_warmup_draws[,1:length(model_to_fit$group_pars)])
m1_group_par_est     <- apply(m1_group_par_samples, MARGIN=2, FUN = median)
m1_group_par_CI      <- apply(m1_group_par_samples, MARGIN = 2, FUN = quantile , probs = c(.025, .5, .975))

## Extract model choices
m1_pred_right_prop <- colMeans(m1_choice_pred$post_warmup_draws)[1:(40*150*2)]

#####################################################
## Extract parameter samples (Individual-level)
#####################################################

m1_indiv_par_samples_all <- read_cmdstan_csv(
  files= output_files,
  variables = model_to_fit$indiv_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)

m1_indiv_par_samples <- vector(mode="list",
                               length=length(model_to_fit$indiv_pars))

m1_indiv_par_est <- matrix(NA, nrow = m1_indiv_par_samples_all$metadata$stan_variable_sizes[[model_to_fit$indiv_pars[1]]],
                           ncol = length(model_to_fit$indiv_pars))
colnames(m1_indiv_par_est) <- model_to_fit$indiv_pars

## Plot distribution for samples of individual participant parameters
for (i in 1:length(m1_indiv_par_samples)){
  m1_indiv_par_samples[[i]] <-
    as.matrix(m1_indiv_par_samples_all$post_warmup_draws[seq(
      from       = 1 + (i-1) * dim(m1_indiv_par_est)[1],
      to         = i * dim(m1_indiv_par_est)[1],
      length.out = dim(m1_indiv_par_est)[1])
    ])
  m1_indiv_par_est[,i] <- apply(m1_indiv_par_samples[[i]], MARGIN=2, FUN=median)
  
  hist(m1_indiv_par_est[,i], main=model_to_fit$indiv_pars[i], 30)
}

## Code to get beta sample distribution for e.g., participant no. 11
pt_id <- "18"
pt_ix <- match(pt_id, sim_data$subject %>% unique())
param <- match("beta", model_q_learning_pp_offset$indiv_pars)  
hist(m1_indiv_par_samples[[param]][, pt_ix], 100, main= paste("Beta  Samples for", pt_id))

## Record median parameter values for each participant
for (pt_ix in 1:n_participants){
  m1_est_beta[pt_ix]       <- m1_indiv_par_samples[[1]][,pt_ix] %>% median()
  m1_est_eta[pt_ix]      <- m1_indiv_par_samples[[2]][,pt_ix] %>% median()
  m1_est_delta_eta[pt_ix] <- m1_indiv_par_samples[[4]][,pt_ix] %>% median()
  
  m1_beta_within_50ci[pt_ix] <- m1_est_beta[pt_ix] > quantile(m1_indiv_par_samples[[1]], 0.25) & m1_est_beta[pt_ix] < quantile(m1_indiv_par_samples[[1]], 0.75)
  m1_beta_within_90ci[pt_ix] <- m1_est_beta[pt_ix] > quantile(m1_indiv_par_samples[[1]], 0.05) & m1_est_beta[pt_ix] < quantile(m1_indiv_par_samples[[1]], 0.95)
  
  m1_eta_within_50ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[2]], 0.25) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[2]], 0.75)
  m1_eta_within_90ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[2]], 0.05) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[2]], 0.95)
  
  m1_delta_eta_within_50ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[4]], 0.25) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[4]], 0.75)
  m1_delta_eta_within_90ci[pt_ix] <- m1_est_eta[pt_ix] > quantile(m1_indiv_par_samples[[4]], 0.05) & m1_est_eta[pt_ix] < quantile(m1_indiv_par_samples[[4]], 0.95)
}


## check calibration
mean(m1_eta_within_50ci); mean(m1_eta_within_90ci)
mean(m1_beta_within_50ci); mean(m1_beta_within_90ci)
mean(m1_delta_eta_within_50ci); mean(m1_delta_eta_within_90ci)

## Correlations with true parameter values
cor.test(unique(sim_data$true_beta), m1_est_beta)
cor.test(unique(sim_data$true_eta_pre), m1_est_eta)
cor.test(unique(sim_data$true_delta_eta), m1_est_delta_eta)

# m1: save model objects --------------------------------------------------

m1_objects     <- grep("m1_",names(.GlobalEnv),value=TRUE)
m1_object_list <- mget(m1_objects)
save(m1_object_list, file = here::here("data", "m1_model_data.Rdata"))
rm(m1_object_list)

## ======================
