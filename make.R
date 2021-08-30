source("R/packages.R")
source("R/functions-02-dualEvo.R")
source("R/functions-03-nzavsMain.R")
source("R/functions-04-nzavsParty.R")
source("R/functions-05-nzavsLong.R")
source("R/functions-06-miniDG.R")
source("R/plan.R")
make(plan, lock_envir = FALSE, garbage_collection = TRUE,
     memory_strategy = "autoclean")