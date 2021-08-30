# custom functions

# power analysis
ch6_runPowerAnalysis <- function(n = 450, nsims = 100) {
  # get previous data for simulation
  d <- 
    read.csv("data/miniDGs/previous/panizzaDG.csv") %>%
    left_join(read.csv("data/miniDGs/previous/panizzaKW.csv") %>% filter(right == 0), 
              by = c("part", "TL", "BL", "TR", "BR")) %>%
    left_join(read.csv("data/miniDGs/previous/panizzaKW.csv") %>% filter(right == 1), 
              by = c("part", "TL", "BL", "TR", "BR")) %>%
    drop_na() %>%
    transmute(
      part        = factor(part),                          # participant id
      type        = factor(paste0(TL, BL, TR, BR)),        # miniDG type
      non_selfish = 1 - selfish,                           # did participant choose unselfish option?
      self_adv    = self_adv,                              # payoffs to dictator - selfish option
      self_dis    = self_dis,                              # payoffs to dictator - non-selfish option
      diffPayoff  = (self_dis - self_adv) / 60,            # difference in payoffs to dictator
      norm_adv    = ifelse(TL > TR, rating.x, rating.y),   # normative rating - selfish option
      norm_dis    = ifelse(TL > TR, rating.y, rating.x),   # normative rating - non-selfish option
      diffNorm    = ordered(round(norm_dis - norm_adv, 2)) # difference in normative rating
    )
  # fit initial model to the complete dataset
  m1 <- brm(non_selfish ~ diffPayoff + mo(diffNorm) + (diffPayoff + mo(diffNorm) | part),
            data = d, family = bernoulli,
            prior = c(prior(normal(0, 1), class = Intercept),
                      prior(normal(0, 1), class = b),
                      prior(exponential(1), class = sd)), 
            iter = 3000, cores = 4, seed = 2113)
  # add random effects parameters (posterior means) to dataset
  getRanMean <- function(par) {
    m1 %>%
      spread_draws(r_part[part,parameter]) %>% 
      group_by(part, parameter) %>% 
      summarise(mean = mean(r_part)) %>% 
      filter(parameter == par) %>%
      mutate(part = factor(part)) %>%
      select(-parameter)
  }
  d <- left_join(d, getRanMean("diffPayoff") %>% rename(beta = mean), by = "part")
  d <- left_join(d, getRanMean("modiffNorm") %>% rename(gamma = mean), by = "part")
  # create function to randomly simulate vector with correlation coefficient
  # from https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables
  complement <- function(y, rho, x) {
    if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
    y.perp <- residuals(lm(x ~ y))
    rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
  }
  # create data simulation function
  simData <- function(seed) {
    # set seed
    set.seed(seed)
    # initialise empty data frame
    out <- data.frame()
    # sample n participants (repeats allowed)
    parts <- sample(1:166, n, replace = TRUE)
    # loop over these participants and add to "out" data.frame
    for (i in 1:n) out <- rbind(out, d %>% filter(part == parts[i]) %>% mutate(part = i))
    # add sdo and rwa, weakly positively correlated with beta and gamma parameters (based on previous research)
    beta <- out %>% group_by(part) %>% summarise(beta = mean(beta)) %>% pull(beta)
    gamma <- out %>% group_by(part) %>% summarise(gamma = mean(gamma)) %>% pull(gamma)
    out <- left_join(out, data.frame(part = 1:n, sdo = complement(beta, 0.24)), by = "part")
    out <- left_join(out, data.frame(part = 1:n, rwa = complement(gamma, 0.2)), by = "part")
    return(out)
  }
  # fit interaction model to first simulated dataset
  m2 <- brm(non_selfish ~ diffPayoff*sdo + mo(diffNorm)*rwa + (diffPayoff + mo(diffNorm) | part),
            data = simData(1), family = bernoulli,
            prior = c(prior(normal(0, 1), class = Intercept),
                      prior(normal(0, 1), class = b),
                      prior(exponential(1), class = sd)), 
            iter = 3000, cores = 4, seed = 2113)
  # summarise model fixed effects function
  summaryModel <- function(model) {summary(model)$fixed}
  # pass_through function for progress printing
  pass_through <- function(data, fun) {fun(data); data}
  # simulate data and fit model nsims times, extract parameters of interest
  s <-
    tibble(seed = 1:nsims) %>%
    mutate(d = map(seed, simData)) %>%
    mutate(pars = map2(d, seed, ~update(m2, newdata = .x, seed = .y, iter = 3000, cores = 4) %>% 
                         summaryModel() %>% 
                         pass_through(function(x) print(paste0("Simulation ", .y))))) %>%
    select(-d) %>% unnest(pars)
  return(s)
}

# read in qualtrics data
ch6_getData <- function() {
  # ordinal sdo / rwa items
  ordNorm <- c("Strongly disagree"          = 1,
               "Disagree"                   = 2,
               "Somewhat disagree"          = 3,
               "Neither agree nor disagree" = 4,
               "Somewhat agree"             = 5,
               "Agree"                      = 6,
               "Strongly agree"             = 7)
  # read data
  out <-
    read_survey("data/miniDGs/Mini+Dictator+Games_February+12,+2021_00.19.csv") %>%
    # keep only finished responses
    filter(Finished) %>%
    # create other variables
    mutate(
      # participant label
      id = seq(1, nrow(.)),
      # bonus for mini dictator games - randomly chose DG13 for payment
      payoffDG = ifelse((id %% 2) != 0, ifelse(DG13 == "35 points to you\n25 points to other participant", 35, 32), NA),
      payoffDG = ifelse((id %% 2) == 0, 60 - lag(payoffDG), payoffDG),
      bonusDG = payoffDG * 0.02,
      # bonus for norm elicitation - randomly chose Norm17 for payment (modal answer = very socially inappropriate)
      bonusNorm = ifelse(Norm17 == "Very socially inappropriate", 0.5, 0),
      # bonus for rf task
      counterbalancingRF = ifelse(rowSums(is.na(select(., starts_with("RF2") & ends_with("RANK")))) == 60, 
                                  "Bucket A breaks the rule", "Bucket B breaks the rule"),
      RF1_followRule = ifelse(counterbalancingRF == "Bucket A breaks the rule",
                              rowSums(!is.na(select(., starts_with("RF1_1") & ends_with("RANK")))), NA),
      RF1_breakRule  = ifelse(counterbalancingRF == "Bucket A breaks the rule",
                              rowSums(!is.na(select(., starts_with("RF1_0") & ends_with("RANK")))), NA),
      RF2_followRule = ifelse(counterbalancingRF == "Bucket B breaks the rule",
                              rowSums(!is.na(select(., starts_with("RF2_0") & ends_with("RANK")))), NA),
      RF2_breakRule  = ifelse(counterbalancingRF == "Bucket B breaks the rule",
                              rowSums(!is.na(select(., starts_with("RF2_1") & ends_with("RANK")))), NA),
      followRule = ifelse(is.na(RF1_followRule), RF2_followRule, RF1_followRule),
      breakRule = ifelse(is.na(RF1_breakRule), RF2_breakRule, RF1_breakRule),
      payoffRF = followRule + (breakRule*2),
      bonusRF = payoffRF * 0.02,
      # fix participant errors in beast task
      BR5B = ifelse(BR5B == 685 , 68 , BR5B),
      # timeout in beast task?
      timeoutBEAST = rowSums(select(., starts_with("timeout")) == "no timeout") != 10,
      # bonus for beast task - randomly chose beast round 3 part a for payment (correct answer = 59)
      payoffBEAST = (100 - (abs(59 - BR3A)*5)),
      payoffBEAST = ifelse(timeoutBEAST, 0, ifelse(payoffBEAST < 0, 0, payoffBEAST)),
      bonusBEAST  = payoffBEAST * 0.02,
      # overall bonus and payment
      bonus = bonusDG + bonusNorm + bonusRF + bonusBEAST,
      payment = 3.75 + bonus
    ) %>%
    # n = 473
    # implement exclusion criteria (no captcha problems or bad verbatims)
    filter(
      Attention1 == 7 &                                                                      # attention check 1
        Attention2 == "h e l l o" &                                                            # attention check 2
        colSums(apply(select(., starts_with("SDORWA")), 1, function(x) duplicated(x))) != 11 & # flatliners (sdo / rwa)
        colSums(apply(select(., starts_with("ESCon_")), 1, function(x) duplicated(x))) != 11 & # flatliners (economic / social)
        `Duration (in seconds)` > 60*4                                                         # speeders
    ) %>%
    # n = 436
    # create more variables
    # sdo / rwa to standardised average
    # items 4, 5, 6, 10, 11, and 12 are reversed
    mutate_at(vars(starts_with("SDORWA")), function(x) as.numeric(ordNorm[x])) %>%
    mutate(sdo = as.numeric(scale((SDORWA_1 + SDORWA_2 + SDORWA_3 + 
                                     (8 - SDORWA_4) + (8 - SDORWA_5) + (8 - SDORWA_6)) / 6)),
           rwa = as.numeric(scale((SDORWA_7 + SDORWA_8 + SDORWA_9 + 
                                     (8 - SDORWA_10) + (8 - SDORWA_11) + (8 - SDORWA_12)) / 6))) %>%
    # economic / social conservatism
    mutate(ec = as.numeric(scale((ESCon_398 + (100 - ESCon_401) + ESCon_402 + ESCon_405 + ESCon_406) / 5)),
           sc = as.numeric(scale(((100 - ESCon_397) + ESCon_399 + ESCon_400 + ESCon_403 + ESCon_404 + ESCon_407 + ESCon_408) / 7)))
  return(out)
}

# pivot data for confirmatory dg analyses
ch6_pivotDataDG <- function(d) {
  # ordinal norm items
  ordNorm <- c("Very socially inappropriate"     = 1,
               "Somewhat socially inappropriate" = 2,
               "Somewhat socially appropriate"   = 3,
               "Very socially appropriate"       = 4)
  # get mode
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  out <-
    d %>%
    # standardise age
    mutate(Age = as.numeric(scale(Age))) %>%
    # pivot dg choices to long format
    pivot_longer(
      cols = starts_with("DG"),
      names_to = "dg",
      values_to = "dgChoice"
    ) %>%
    # norm items to numeric
    mutate_at(vars(starts_with("Norm")), function(x) as.numeric(ordNorm[x])) %>%
    mutate(
      # dgChoice as numeric
      dgChoice = parse_number(dgChoice),
      # non-selfish option (binary)
      nonself = ifelse(dgChoice == rep(c(0 , 15, 21, 25, 26, 27, 
                                         28, 30, 30, 30, 30, 31,
                                         32, 32, 33, 33, 34, 45), 
                                       times = nrow(d)), 1, 0),
      # difference in payoffs to dictator
      diffP = rep(c(-60, -40, -36, -30, -9 , -6 ,
                    -9 , -4 , -9 , -30, -15, -6 ,
                    -3 , -25, -1 , -12, -21, -15), times = nrow(d)) / 60,
      # difference in differences in payoffs between dictator and receiver (i.e. difference in equality)
      diffE = rep(c(0 , 20, 36, 40, 2 , 0 ,
                    10, 8 , 18, 60, 30, 12,
                    6 , 50, 2 , 24, 42, 30), times = nrow(d)) / 60,
      # difference in normative evaluation
      diffN = NA,
      diffN = ifelse(dg == "DG01", Norm01 - Norm19, diffN),
      diffN = ifelse(dg == "DG02", Norm02 - Norm17, diffN),
      diffN = ifelse(dg == "DG03", Norm03 - Norm18, diffN),
      diffN = ifelse(dg == "DG04", Norm04 - Norm17, diffN),
      diffN = ifelse(dg == "DG05", Norm05 - Norm13, diffN),
      diffN = ifelse(dg == "DG06", Norm06 - Norm11, diffN),
      diffN = ifelse(dg == "DG07", Norm07 - Norm14, diffN),
      diffN = ifelse(dg == "DG08", Norm08 - Norm12, diffN),
      diffN = ifelse(dg == "DG09", Norm08 - Norm15, diffN),
      diffN = ifelse(dg == "DG10", Norm08 - Norm19, diffN),
      diffN = ifelse(dg == "DG11", Norm08 - Norm16, diffN),
      diffN = ifelse(dg == "DG12", Norm09 - Norm14, diffN),
      diffN = ifelse(dg == "DG13", Norm10 - Norm13, diffN),
      diffN = ifelse(dg == "DG14", Norm10 - Norm18, diffN),
      diffN = ifelse(dg == "DG15", Norm11 - Norm12, diffN),
      diffN = ifelse(dg == "DG16", Norm11 - Norm16, diffN),
      diffN = ifelse(dg == "DG17", Norm12 - Norm17, diffN),
      diffN = ifelse(dg == "DG18", Norm16 - Norm19, diffN),
      # difference in modal normative evaluation
      diffNmode = NA,
      diffNmode = ifelse(dg == "DG01", getmode(Norm01) - getmode(Norm19), diffNmode),
      diffNmode = ifelse(dg == "DG02", getmode(Norm02) - getmode(Norm17), diffNmode),
      diffNmode = ifelse(dg == "DG03", getmode(Norm03) - getmode(Norm18), diffNmode),
      diffNmode = ifelse(dg == "DG04", getmode(Norm04) - getmode(Norm17), diffNmode),
      diffNmode = ifelse(dg == "DG05", getmode(Norm05) - getmode(Norm13), diffNmode),
      diffNmode = ifelse(dg == "DG06", getmode(Norm06) - getmode(Norm11), diffNmode),
      diffNmode = ifelse(dg == "DG07", getmode(Norm07) - getmode(Norm14), diffNmode),
      diffNmode = ifelse(dg == "DG08", getmode(Norm08) - getmode(Norm12), diffNmode),
      diffNmode = ifelse(dg == "DG09", getmode(Norm08) - getmode(Norm15), diffNmode),
      diffNmode = ifelse(dg == "DG10", getmode(Norm08) - getmode(Norm19), diffNmode),
      diffNmode = ifelse(dg == "DG11", getmode(Norm08) - getmode(Norm16), diffNmode),
      diffNmode = ifelse(dg == "DG12", getmode(Norm09) - getmode(Norm14), diffNmode),
      diffNmode = ifelse(dg == "DG13", getmode(Norm10) - getmode(Norm13), diffNmode),
      diffNmode = ifelse(dg == "DG14", getmode(Norm10) - getmode(Norm18), diffNmode),
      diffNmode = ifelse(dg == "DG15", getmode(Norm11) - getmode(Norm12), diffNmode),
      diffNmode = ifelse(dg == "DG16", getmode(Norm11) - getmode(Norm16), diffNmode),
      diffNmode = ifelse(dg == "DG17", getmode(Norm12) - getmode(Norm17), diffNmode),
      diffNmode = ifelse(dg == "DG18", getmode(Norm16) - getmode(Norm19), diffNmode),
    )
  return(out)
}

# pivot data for exploratory norm analyses
ch6_pivotDataNorm <- function(d) {
  # ordinal norm items
  ordNorm <- c("Very socially inappropriate"     = 1,
               "Somewhat socially inappropriate" = 2,
               "Somewhat socially appropriate"   = 3,
               "Very socially appropriate"       = 4)
  out <-
    d %>%
    # standardise age
    mutate(Age = as.numeric(scale(Age))) %>%
    # norm items to numeric
    mutate_at(vars(starts_with("Norm")), function(x) as.numeric(ordNorm[x])) %>%
    # pivot norm choices to long format
    pivot_longer(
      cols = starts_with("Norm"),
      names_to = "normItem",
      values_to = "norm"
    )
  return(out)
}

# pivot data for exploratory rule following analysis
ch6_pivotDataRF <- function(d) {
  d %>%
    # standardise age
    mutate(Age = as.numeric(scale(Age))) %>%
    # norm items to numeric
    mutate_at(vars(starts_with("RF") & ends_with("RANK")), function(x) ifelse(!is.na(x), 1, NA)) %>%
    # pivot rf choices to long format
    pivot_longer(cols = starts_with("RF") & ends_with("RANK"), values_to = "binaryRuleFollow") %>%
    # separate new column
    separate(name, sep = "_", into = c("rf", "rfBucket", "rfRound", "rfRank")) %>%
    # filter only relevant rows
    filter((counterbalancingRF == "Bucket A breaks the rule" & rf == "RF1" & rfBucket == "1") | 
             (counterbalancingRF == "Bucket B breaks the rule" & rf == "RF2" & rfBucket == "0")) %>%
    # clean up
    mutate(binaryRuleFollow = ifelse(is.na(binaryRuleFollow), 0, binaryRuleFollow),
           rfRound = (as.numeric(rfRound) - 1) / 29) %>%
    select(-rfBucket, -rfRank)
}

# pre-registered model 0
ch6_fitModel0 <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept,
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b)),
        cores = 4, seed = 2113)
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# pre-registered model 1
ch6_fitModel1 <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP + mo(diffN) + (1 + diffP + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)),
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.85))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# pre-registered model 2a (no controls)
ch6_fitModel2a <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP*sdo + mo(diffN)*rwa + (1 + diffP + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.9))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# pre-registered model 2b (with controls)
ch6_fitModel2b <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP*sdo + mo(diffN)*rwa + Gender + Age + (1 + diffP + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.85))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 2c
ch6_fitModel2c <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP*sdo + mo(diffN) + (1 + diffP + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.9))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 2d
ch6_fitModel2d <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP + mo(diffN)*rwa + (1 + diffP + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.9))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 3
ch6_fitModel3 <- function(d) {
  out <- 
    brm(norm ~ 1 + sdo + rwa + (1 | id) + (1 + sdo + rwa | normItem),
        data = d, family = cumulative,
        prior = c(prior(normal(0, 2), class = Intercept), # prior choices from prior predictive checks
                  prior(normal(0, 0.5), class = b),       # these priors give every ordinal outcome (1-4)
                  prior(exponential(4), class = sd)),     # equal prior plausibility
        iter = 3000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 4
ch6_fitModel4 <- function(d) {
  d$followRule <- d$followRule / 30
  out <-
    brm(nonself ~ 0 + Intercept + diffP + mo(diffN)*followRule + (1 + diffP + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 8000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.85))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 5
ch6_fitModel5 <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + sdo + rwa + (1 | id) + (1 + sdo + rwa | dg),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.9))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 6
ch6_fitModel6 <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP*sdo + mo(diffNmode)*rwa + (1 + diffP + mo(diffNmode) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.85))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 7
ch6_fitModel7 <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP + diffE + mo(diffN) + (1 + diffP + diffE + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.85))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 8
ch6_fitModel8 <- function(d) {
  out <-
    brm(nonself ~ 0 + Intercept + diffP + diffE*sdo + mo(diffN)*rwa + (1 + diffP + diffE + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 6000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.9))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 9
ch6_fitModel9 <- function(d) {
  out <-
    brm(binaryRuleFollow ~ 0 + Intercept + rfRound + sdo + rwa + (1 + rfRound | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 0.5), class = b),
                  prior(exponential(3), class = sd),
                  prior(lkj_corr_cholesky(2), class = L)),
        iter = 3000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.99))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 10
ch6_fitModel10 <- function(d) {
  # diffN as ordered factor for modelling
  d$diffN <- ordered(d$diffN)
  out <- 
    brm(diffN ~ 1 + sdo + rwa + (1 | id) + (1 + sdo + rwa | dg),
        data = d, family = cumulative,
        prior = c(prior(normal(0, 2), class = Intercept), # prior choices from prior predictive checks
                  prior(normal(0, 0.5), class = b),       # these priors give every ordinal outcome (-3 - 3)
                  prior(exponential(4), class = sd)),     # equal prior plausibility
        iter = 3000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# exploratory model 11
ch6_fitModel11 <- function(d) {
  d$followRule <- d$followRule / 30
  out <-
    brm(nonself ~ 0 + Intercept + diffP*followRule + mo(diffN) + (1 + diffP + mo(diffN) | id),
        data = d, family = bernoulli,
        prior = c(prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sd)), 
        iter = 8000, cores = 4, seed = 2113,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = 0.85))
  out <- add_criterion(out, c("waic", "loo"))
  return(out)
}

# fit separate models for each individual
ch6_fitIndModels <- function(dDG) {
  # fit model to first set to later use update()
  m <- brm(nonself ~ 0 + Intercept + diffP + mo(diffN),
           data = dDG[dDG$id == 1,], family = bernoulli,
           prior = c(prior(normal(0, 1), class = b)),
           iter = 6000, cores = 4, seed = 2113,
           save_pars = save_pars(all = TRUE),
           control = list(adapt_delta = 0.85))
  # repeat with all sets
  out <- 
    dDG %>%
    # models for id 10 and 458 fail
    filter(id != 10 & id != 458) %>%
    group_by(id) %>%
    nest() %>%
    mutate(
      model     = map(data,  function(x) update(m, newdata = x)),
      diffP_est = map(model, function(x) fixef(x)["diffP","Estimate"]),
      diffP_se  = map(model, function(x) fixef(x)["diffP","Est.Error"]),
      diffN_est = map(model, function(x) fixef(x)["modiffN","Estimate"]),
      diffN_se  = map(model, function(x) fixef(x)["modiffN","Est.Error"])
    ) %>%
    unnest(c(diffP_est, diffP_se, diffN_est, diffN_se)) %>%
    select(-model, -data)
  return(out)
}

# fit model predicting separate diff pars
ch6_fitIndDiff <- function(d, indModels, par, parSE, se = FALSE) {
  # model formula
  f <- bf(paste0(par, ifelse(se, paste0(" | se(", parSE, ", sigma = TRUE)"), ""),
                 " ~ 0 + Intercept + sdo + rwa"))
  # fit model
  out <-
    left_join(d, indModels, by = "id") %>%
    brm(formula = f, data = .,
        prior = prior(normal(0, 1), class = b))
  return(out)
}

# create plot of non-selfish dg choices and normative evaluations
ch6_createCoopNormPlot <- function(dDG, dNorm) {
  # coop plot
  pA <-
    dDG %>%
    group_by(dg) %>%
    summarise(nonself = mean(nonself)) %>%
    mutate(dg = c(
      "0/60 or 60/0"  , "15/45 or 55/5" , "21/39 or 57/3" ,
      "25/35 or 55/5" , "26/34 or 35/25", "27/33 or 33/27",
      "28/32 or 37/23", "30/30 or 34/26", "30/30 or 39/21",
      "30/30 or 60/0 ", "30/30 or 45/15", "31/29 or 37/23",
      "32/28 or 35/25", "32/28 or 57/3" , "33/27 or 34/26",
      "33/27 or 45/15", "34/26 or 55/5" , "45/15 or 60/0 "
    )) %>%
    ggplot(aes(x = fct_reorder(dg, nonself), y = nonself)) +
    geom_col(fill = "#0072B2") +
    labs(x = NULL, y = "Proportion choosing non-selfish option") +
    ylim(c(0, 1)) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
  # norm plot
  normItems <- c(0 , 15, 21, 25, 26, 27, 28, 30, 31, 
                 32, 33, 34, 35, 37, 39, 45, 55, 57, 60)
  pB <-
    dNorm %>%
    mutate(normItem = rep(normItems, times = nrow(.) / length(normItems))) %>%
    group_by(normItem) %>%
    summarise(mean  = mean(norm),
              se    = sd(norm) / sqrt(n()),
              upper = mean + 2*se,
              lower = mean - 2*se) %>%
    ggplot(aes(x = normItem, y = mean, ymin = lower, ymax = upper)) +
    geom_line(alpha = 0.5, colour = "grey") +
    geom_pointrange(size = 0.25) +
    scale_x_continuous(name = "Payoff to dictator (points)", breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(name = "Mean normative appropriateness rating", limits = c(1, 4))
  # put together
  out <- plot_grid(pA, pB, nrow = 2, labels = letters[1:2])
  return(out)
}

# plot sdo effect on each mini dg item
ch6_createSDOPlot <- function(model) {
  conditions <- data.frame(dg = unique(model$data$dg))
  rownames(conditions) <- c("0/60 or 60/0"  , "15/45 or 55/5" ,
                            "21/39 or 57/3" , "25/35 or 55/5" ,
                            "26/34 or 35/25", "27/33 or 33/27",
                            "28/32 or 37/23", "30/30 or 34/26",
                            "30/30 or 39/21", "30/30 or 60/0" ,
                            "30/30 or 45/15", "31/29 or 37/23",
                            "32/28 or 35/25", "32/28 or 57/3" ,
                            "33/27 or 34/26", "33/27 or 45/15",
                            "34/26 or 55/5" , "45/15 or 60/0")
  cond <- conditional_effects(model, effects = "sdo", conditions = conditions, re_formula = . ~ (1 + sdo + rwa | dg))
  out <- 
    plot(cond, ncol = 5, plot = FALSE, line_args = list(colour = "black"))[[1]] +
    labs(x = "Social Dominance Orientation", y = "Probability of choosing non-selfish option")
  return(out)
}

# plot rwa effect on each norm item
ch6_createRWAPlot <- function(model) {
  conditions <- data.frame(normItem = unique(model$data$normItem))
  rownames(conditions) <- c("0 to you\n60 to other" , "15 to you\n45 to other",
                            "21 to you\n39 to other", "25 to you\n35 to other",
                            "26 to you\n34 to other", "27 to you\n33 to other",
                            "28 to you\n32 to other", "30 to you\n30 to other",
                            "31 to you\n29 to other", "32 to you\n28 to other",
                            "33 to you\n27 to other", "34 to you\n26 to other",
                            "35 to you\n25 to other", "37 to you\n23 to other",
                            "39 to you\n21 to other", "45 to you\n15 to other",
                            "55 to you\n5 to other" , "57 to you\n3 to other" ,
                            "60 to you\n0 to other")
  cond <- conditional_effects(model, effects = "rwa", conditions = conditions, re_formula = . ~ (1 + sdo + rwa | normItem))
  out <- 
    plot(cond, ncol = 5, plot = FALSE, line_args = list(colour = "black"))[[1]] +
    labs(x = "Right Wing Authoritarianism", y = "Social appropriateness rating\n(1 = very socially inappropriate, 4 = very socially appropriate)")
  return(out)
}

# plot confirmatory analyses
ch6_createMainPlot <- function(d, model, post, xvar1, xvar2, xlab1, xlab2, filename) {
  # generic plotting function
  plotFun <- function(d, model, post, xvar, yvar, xlab, ylab, colour) {
    # posterior predictions
    predFun <- function(x) {
      post[,paste0("b", ifelse(yvar == "modiffN", "sp", ""), "_", yvar)] + 
        post[,paste0("b", ifelse(yvar == "modiffN", "sp", ""), "_", yvar, ":", xvar)]*x
    }
    pred <- 
      tibble(x = seq(min(d[,xvar]), max(d[,xvar]), length.out = 1000)) %>%
      mutate(y  = apply(., 1, function(x) median(predFun(x))),
             hi = apply(., 1, function(x) quantile(predFun(x), 0.975)),
             lo = apply(., 1, function(x) quantile(predFun(x), 0.025)))
    # yvar
    yvarSym <- sym(paste0("b", ifelse(yvar == "modiffN", "sp", ""), "_", yvar))
    # plot
    model %>%
      spread_draws(!!yvarSym, r_id[id,parameter]) %>%
      group_by(id, parameter) %>%
      summarise(mean = mean(!!yvarSym + r_id)) %>%
      filter(parameter == yvar) %>%
      left_join(d[,c("id", xvar)], by = "id") %>%
      ggplot(aes(x = !!sym(xvar), y = mean)) +
      geom_jitter(alpha = 0.6, colour = colour) +
      geom_ribbon(data = pred, aes(x = x, y = y, ymin = lo, ymax = hi),
                  fill = "grey", alpha = 0.5) +
      geom_line(data = pred, aes(x = x, y = y)) +
      labs(x = xlab, y = ylab)
  }
  # separate plots
  pA <- plotFun(d, model, post, xvar1, "diffP", xlab1, expression(atop(paste("Payoff utility component (", beta, ")"), "less payoff-maximising" %<->% "more payoff-maximising")), "#0072B2")
  pB <- plotFun(d, model, post, xvar2, "modiffN", xlab2, expression(atop(paste("Normative utility component (", gamma, ")"), "less norm adherence" %<->% "more norm adherence")), "#D55E00")
  # put together
  out <- plot_grid(pA, pB, labels = c("a","b"))
  # save plot
  return(out)
}

# plot utility parameters
ch6_createUtilityPlot <- function(model) {
  # conditional effects
  cond <- conditional_effects(model)
  # plot A
  pA <-
    ggplot(cond$diffP, aes(x = diffP, y = estimate__)) +
    geom_ribbon(aes(x = diffP, ymin = lower__, ymax = upper__),
                fill = "grey") +
    geom_line() +
    scale_x_continuous(name = "Difference in payoffs to dictator between\nnon-selfish and selfish options (points)", labels = function(x) x*60) +
    scale_y_continuous(name = "Probability of choosing non-selfish option", limits = c(0, 1))
  # plot B
  pB <-
    ggplot(cond$diffN, aes(x = diffN, y = estimate__)) +
    geom_ribbon(aes(x = diffN, ymin = lower__, ymax = upper__),
                fill = "grey") +
    geom_line() +
    scale_x_continuous(name = "Difference in normative appropriateness ratings\nbetween non-selfish and selfish options") +
    scale_y_continuous(name = " ", limits = c(0, 1))
  # put together
  out <- plot_grid(pA, pB, labels = c("a", "b"))
  return(out)
}

# variation in pars across participants
ch6_createUtilityVarPlot <- function(model) {
  # loop over participants
  ids <- unique(model$data$id)
  varDiffP <- data.frame()
  varDiffN <- data.frame()
  for (i in ids) {
    conditions <- data.frame(id = i)
    rownames(conditions) <- i
    cond <- conditional_effects(model, conditions = conditions, re_formula = NULL)
    varDiffP <- rbind(varDiffP, cond$diffP)
    varDiffN <- rbind(varDiffN, cond$diffN)
  }
  # plot A
  pA <-
    ggplot(varDiffP, aes(x = diffP, y = estimate__, group = id)) +
    geom_line(colour = "grey", alpha = 0.4, size = 0.1) +
    scale_x_continuous(name = "Difference in payoffs to dictator between\nnon-selfish and selfish options (points)", labels = function(x) x*60) +
    scale_y_continuous(name = "Probability of choosing non-selfish option", limits = c(0, 1))
  # plot B
  pB <-
    ggplot(varDiffN, aes(x = diffN, y = estimate__, group = id)) +
    geom_line(colour = "grey", alpha = 0.4, size = 0.1) +
    scale_x_continuous(name = "Difference in normative appropriateness ratings\nbetween non-selfish and selfish options") +
    scale_y_continuous(name = " ", limits = c(0, 1))
  # put together
  out <- plot_grid(pA, pB, labels = c("a", "b"))
  return(out)
}

# table of mini dg items
ch6_createMiniDGTable <- function() {
  tibble(
    `Mini-DG item` = 1:18,
    `Option 1` = c(
      "0 to you, 60 to other" , "15 to you, 45 to other", 
      "21 to you, 39 to other", "25 to you, 35 to other", 
      "26 to you, 34 to other", "27 to you, 33 to other", 
      "28 to you, 32 to other", "30 to you, 30 to other", 
      "30 to you, 30 to other", "30 to you, 30 to other", 
      "30 to you, 30 to other", "31 to you, 29 to other", 
      "32 to you, 28 to other", "32 to you, 28 to other", 
      "33 to you, 27 to other", "33 to you, 27 to other", 
      "34 to you, 26 to other", "45 to you, 15 to other"
    ),
    `Option 2` = c(
      "60 to you, 0 to other" , "55 to you, 5 to other" , 
      "57 to you, 3 to other" , "55 to you, 5 to other" , 
      "35 to you, 25 to other", "33 to you, 27 to other", 
      "37 to you, 23 to other", "34 to you, 26 to other", 
      "39 to you, 21 to other", "60 to you, 0 to other" , 
      "45 to you, 15 to other", "37 to you, 23 to other", 
      "35 to you, 25 to other", "57 to you, 3 to other" , 
      "34 to you, 26 to other", "45 to you, 15 to other", 
      "55 to you, 5 to other" , "60 to you, 0 to other"
    )
  )
}

# table of self-report items
ch6_createSelfReportTable <- function() {
  tibble(
    Item = 
      c("SDO1","SDO2","SDO3","SDO4r","SDO5r","SDO6r",
        "RWA1","RWA2","RWA3","RWA4r","RWA5r","RWA6r",
        "PolParty","Education","Age","Gender"),
    `Description / Text` = 
      c("It is OK if some groups have more of a chance in life than others",
        "Inferior groups should stay in their place",
        "To get ahead in life, it is sometimes okay to step on other groups",
        "We should have increased social equality",
        "It would be good if groups could be equal",
        "We should do what we can to equalise conditions for different groups",
        "It is always better to trust the judgment of the proper authorities in government and religion than to listen to the noisy rabble-rousers in our society who are trying to create doubt in people's minds",
        "It would be best for everyone if the proper authorities censored magazines so that people could not get their hands on trashy and disgusting material",
        "Our country will be destroyed some day if we do not smash the perversions eating away at our moral fibre and traditional beliefs",
        "People should pay less attention to The Bible and other old traditional forms of religious guidance, and instead develop their own personal standards of what is moral and immoral",
        "Atheists and others who have rebelled against established religions are no doubt every bit as good and virtuous as those who attend church regularly",
        "Some of the best people in our country are those who are challenging our government, criticizing religion, and ignoring the `normal way' things are supposed to be done",
        "Political party support",
        "What is the highest level of school you have completed or the highest degree you have received?",
        "What is your age in years?",
        "What is your gender?"),
    Type = c(rep("Ordinal (1-7)", 12),
             "Categorical",
             "Ordinal (1-8)",
             "Numeric (18-110)",
             "Categorical")
  )
}

# inequality aversion
ch6_createInequalityAversionPlot <- function(model) {
  # conditional effects
  cond <- conditional_effects(model)
  # plot
  out <-
    ggplot(cond$diffE, aes(x = diffE, y = estimate__)) +
    geom_ribbon(aes(x = diffE, ymin = lower__, ymax = upper__),
                fill = "grey") +
    geom_line() +
    scale_x_continuous(name = "Difference in equality between selfish\nand non-selfish options (0 = no difference,\n1 = non-selfish option more equal)") +
    scale_y_continuous(name = "Probability of choosing non-selfish option", limits = c(0, 1))
  # save
  return(out)
}

# variation in inequality aversion across participants
ch6_createInequalityAversionVarPlot <- function(model) {
  # loop over participants
  ids <- unique(model$data$id)
  varDiffE <- data.frame()
  for (i in ids) {
    conditions <- data.frame(id = i)
    rownames(conditions) <- i
    cond <- conditional_effects(model, conditions = conditions, re_formula = NULL)
    varDiffE <- rbind(varDiffE, cond$diffE)
  }
  # plot
  out <-
    ggplot(varDiffE, aes(x = diffE, y = estimate__, group = id)) +
    geom_line(colour = "grey", alpha = 0.4, size = 0.1) +
    scale_x_continuous(name = "Difference in equality between selfish\nand non-selfish options (0 = no difference,\n1 = non-selfish option more equal)") +
    scale_y_continuous(name = "Probability of choosing non-selfish option", limits = c(0, 1))
  return(out)
}

# plot exploratory inequality aversion analyses
ch6_createInequalityAversionSDOPlot <- function(d, model, post) {
  # posterior predictions
  predFun <- function(x) post[,"b_diffE"] + post[,"b_diffE:sdo"]*x
  pred <- 
    tibble(x = seq(min(d[,"sdo"]), max(d[,"sdo"]), length.out = 1000)) %>%
    mutate(y  = apply(., 1, function(x) median(predFun(x))),
           hi = apply(., 1, function(x) quantile(predFun(x), 0.975)),
           lo = apply(., 1, function(x) quantile(predFun(x), 0.025)))
  # plot
  out <-
    model %>%
    spread_draws(b_diffE, r_id[id,parameter]) %>%
    group_by(id, parameter) %>%
    summarise(mean = mean(b_diffE + r_id)) %>%
    filter(parameter == "diffE") %>%
    left_join(d[,c("id", "sdo")], by = "id") %>%
    ggplot(aes(x = sdo, y = mean)) +
    geom_jitter(alpha = 0.6, height = 1, colour = "#30BE00") +
    geom_ribbon(data = pred, aes(x = x, y = y, ymin = lo, ymax = hi),
                fill = "grey", alpha = 0.5) +
    geom_line(data = pred, aes(x = x, y = y)) +
    labs(x = "Social Dominance Orientation", y = expression(atop(paste("Inequality aversion utility component (", delta, ")"), "less inequality aversion" %<->% "more inequality aversion")))
  return(out)
}
