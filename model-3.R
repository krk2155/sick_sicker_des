# Model 3 consists of healthy -> sick1 -> sick2 -> dead  
# + time in model
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
source('c:/Users/kimkr3/OneDrive - Vanderbilt/DES_Limited_LTSS/sick_sicker_des/event_death.R')  # Death event
  
# Resource or "Counters"
#
# These are used to track things that incur costs or qalys, or other
# things of which a count might be of interest.
# Infinite in quantity
counters <- c(
  "time_in_model", "death", "Healthy","Sick1","Sick2"
)
  
# Define starting state of patient
initialize_patient <- function(traj, inputs)
{
  traj                   |>
  seize("time_in_model") |>
  set_attribute("AgeInitial", function() sample(20:30, 1))
}

# Cleanup function if a termination occurs
# Good for releasing any seized resources based on state.
cleanup_on_termination <- function(traj)
{
  traj |> 
  release("time_in_model")
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
       time_to_event = function(inputs) years_till_death(inputs),
       func          = death,
       reactive      = (FALSE)
       ),
    list(name          = "Healthy to Sick1",
       attr          = "aHealthyToSick1",
       time_to_event = function(inputs) inputs$healthy_to_sick1,
       func          = healthy_to_sick1,
       reactive      = (TRUE)
       ),
    list(name          = "Sick1 to Sick2",
         attr          = "aSick1ToSick2",
         time_to_event = function(inputs) inputs$sick1_to_sick2,
         func          = sick1_to_sick2,
         reactive      = (TRUE)
         ),
    list(name          = "Sick2 to Death",
            attr          = "aSick2ToDeath",
            time_to_event = function(inputs) inputs$sick2_to_death,
            func          = sick2_to_death,
            reactive      = (TRUE)
            )
    
)
       

cost_arrivals <- function(arrivals, inputs)
{
  arrivals$cost  <- 0  # No costs yet
  arrivals$dcost <- 0  # No discounted costs either
  
  selector <- arrivals$resource == 'time_in_model'
  arrivals$cost[selector] <- 0*(arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(0, 
                                            arrivals$start_time[selector],
                                            arrivals$end_time[selector])

  selector <- arrivals$resource == 'death'
  arrivals$cost[selector] <- 0*(arrivals$end_time[selector] - arrivals$start_time[selector])
  arrivals$dcost[selector] <- discount_value(0, 
                                            arrivals$start_time[selector],
                                            arrivals$end_time[selector])
  
  arrivals
}

qaly_arrivals <- function(arrivals, inputs)
{
  arrivals$qaly  <- 0  # No qaly yet
  arrivals$dqaly <- 0  # No discounted qaly either
  
  selector <- arrivals$resource == 'time_in_model'
  arrivals$qaly[selector] <- arrivals$end_time[selector] - arrivals$start_time[selector]
  arrivals$dqaly[selector] <- discount_value(1, 
                                            arrivals$start_time[selector],
                                            arrivals$end_time[selector])

  selector <- arrivals$resource == 'death'
  arrivals$qaly[selector] <- arrivals$end_time[selector] - arrivals$start_time[selector]
  arrivals$dqaly[selector] <- discount_value(1, 
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

des_run(inputs)
get_mon_attributes(env)
