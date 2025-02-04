# Model 6
# Objective: Adding treatment
#############################################################################
#
#
# Copyright 2015, 2025 Shawn Garbett, Vanderbilt University Medical Center
#
# Permission to use, copy, modify, distribute, and sell this software and
# its documentation for any purpose is hereby granted without fee,
# provided that the above copyright notice appear in all copies and that
# both that copyright notice and this permission notice appear in
# supporting documentation. No representations are made about the
# suitability of this software for any purpose.  It is provided "as is"
# without express or implied warranty.
#
###############################################################################

source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/discount.R')
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/inputs.R')     # Your Model Parameters
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/main_loop.R')  # Boilerplate code
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/event_sick1.R')
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/event_sick2.R') 
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/event_death2.R') 
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/event_death3.R')  # Death event
  
#library(simmer)

# Resource or "Counters"
#
# These are used to track things that incur costs or qalys, or other
# things of which a count might be of interest.
# Infinite in quantity
counters <- c(
  "time_in_model", "death", "healthy","sick1", "sick2", "treat"
)
  
# Define starting state of patient
initialize_patient <- function(traj, inputs)
{
  traj                   |>
  seize("time_in_model") |>
  set_attribute("AgeInitial", function() sample(20:30, 1)) |>
  set_attribute("State", 0) |> # Patients start healthy
  seize("healthy")       |>
  set_attribute("Treat", function() inputs$strategy == 'treat') # when does inputs$strategy change to "treat"?
}



# Cleanup function if a termination occurs
# Good for releasing any seized resources based on state.
cleanup_on_termination <- function(traj, inputs)
{
  traj |> 
  release("time_in_model") |>
  branch( 
    function() get_attribute(env, "State")+1,
      continue = rep(TRUE, 3),
      trajectory() |> release("healthy"),
      trajectory() |> release("sick1"),
      trajectory() |> release("sick2") 
  ) |>
  branch( # Assigned to treatment and not healthy
    function() (get_attribute(env, "Treat")  && 
                get_attribute(env, "State")) + 1,
    continue = rep(TRUE, 2),
    trajectory(),  # No Treatment
    trajectory() |> release('treat')
  )
}


terminate_simulation <- function(traj, inputs)
{
  traj |>
  branch( function() 1, 
          continue=FALSE,
          trajectory() |> cleanup_on_termination()
        )
}


# Main Event registry
event_registry <- list(
  list(name          = "Terminate at time horizon",
       attr          = "aTerminate",
       time_to_event = function(inputs) inputs$horizon-now(env),
       func          = terminate_simulation,
       reactive      = (FALSE)),
  list(name          = "Time to Death",
       attr          = "aDeath",
       time_to_event = years_till_death,
       func          = death,
       reactive      = (TRUE)
       ),
  list(name          = "Sick1",
       attr          = "aSick1",
       time_to_event = years_till_sick1,
       func          = sick1,
       reactive      = (TRUE)
       ),
  list(name          = "Sick2",
       attr          = "aSick2",
       time_to_event = years_till_sick2,
       func          = sick2,
       reactive      = (TRUE)
       ),
  list(name          = "Healthy",
       attr          = "aHealthy",
       time_to_event = years_till_healthy,
       func          = healthy,
       reactive      = (TRUE)
       )
)
       

cost_arrivals <- function(arrivals, inputs)
{
  arrivals$cost  <- 0  # No costs yet
  arrivals$dcost <- 0  # No discounted costs either
  
  selector = arrivals$resource == 'healthy' # Why not use "If else" statement between healthy and sick1 states?
  arrivals$cost[selector] <- inputs$c.H *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.H,
    arrivals$start_time[selector], arrivals$end_time[selector])
  
  selector = arrivals$resource == 'sick1'
  arrivals$cost[selector] <- inputs$c.S1 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.S1,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector = arrivals$resource == 'sick2'
  arrivals$cost[selector] <- inputs$c.S2 *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.S2,
    arrivals$start_time[selector], arrivals$end_time[selector])

  selector = arrivals$resource == 'treat'
  arrivals$cost[selector] <- inputs$c.Trt *
    (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.Trt,
    arrivals$start_time[selector], arrivals$end_time[selector])

  arrivals
}

qaly_arrivals <- function(arrivals, inputs)
{
  arrivals$qaly  <- 0  # No qaly yet
  arrivals$dqaly <- 0  # No discounted qaly either

  selector <- arrivals$resource == 'healthy'
  arrivals$qaly[selector] <-
    inputs$u.H*
     (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- 
      discount_value(inputs$u.H, 
                     arrivals$start_time[selector],
                     arrivals$end_time[selector])
  
  selector <- arrivals$resource == 'sick1'
  arrivals$qaly[selector] <-
    inputs$u.S1*
     (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- 
      discount_value(inputs$u.S1, 
                     arrivals$start_time[selector],
                     arrivals$end_time[selector])

  selector <- arrivals$resource == 'sick2'
  arrivals$qaly[selector] <-
    inputs$u.S2*
     (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- 
      discount_value(inputs$u.S2, 
                     arrivals$start_time[selector],
                     arrivals$end_time[selector])

  uS1 <- if(inputs$strategy == 'treat') inputs$u.Trt else inputs$u.S1
  arrivals$qaly[selector] <-
    uS1*
     (arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dqaly[selector] <- 
      discount_value(uS1, 
                     arrivals$start_time[selector],
                     arrivals$end_time[selector])

  arrivals
}

# This does a single DES run versus the defined inputs.
des_run <- function(inputs)
{
  env  <<- simmer("SickSicker")
  traj <- des(env, inputs)
  env |> 
    create_counters(counters) |>
    add_generator("patient", traj, at(rep(0, inputs$N)), mon=2) |>
    run(inputs$horizon+1/365) |> # Simulate just past horizon (in years)
    wrap()
        
  get_mon_arrivals(env, per_resource = T) |>
    cost_arrivals(inputs) |> 
    qaly_arrivals(inputs) 
}

set.seed(4)
# Set the strategy to 'treat' scenario;
# Model-6.R stands as a 'no-treat' scenario
inputs$strategy <- 'treat'
results <- des_run(inputs)

results

# store the result in an excel spreadsheet
write.xlsx(results, file = paste0("Model-7_results_", Sys.Date(), ".xlsx"))

# Example usage:
individual_id <- 2
individual_arrivals(results, individual_id)


# Check Individual Attributes
    # individual_attributes <- function(individual_id) {
    #     attrs <- get_mon_attributes(env)
    #     attrs[attrs$name == paste0("patient", individual_id), ]
    # }

    # head(individual_attributes(4))

    # individual_arrivals <- function(arrivals, individual_id) {
    #     arrivals[arrivals$name == paste0("patient", individual_id), ]
    # }

# NOTE: 20250203 -- there's something odd about the code below; returns NA.
    # set.seed(2)
    # results <- des_run(inputs)
    # pn <- results$name[results$resource == 'sick2'][1]
    # audit <- results[results$name == pn,]
    # audit[order(audit$start_time, audit$end_time),]