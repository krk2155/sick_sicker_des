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

# Given attributes of a patient (trajectory), it returns in years 
# how long till the patient would die a secular death.
#
# This is a terrible mortality model, but is the reference
# from sicker as a simple exponential draw
years_till_sick1 <- function(inputs)
{
  state <- get_attribute(env, "State") 
  if(state == 0) # 0 => Healthy
  {
    rexp(1,inputs$r.HS1)
  } else
  {
    inputs$horizon+1 # Past end of simulation time
  }
}

years_till_death <- function(inputs)
# identifies the state and returns the time to event
{
  state <- get_attribute(env, "State")
  rate <- inputs$r.HD
  if(state == 1) rate <- rate * inputs$hr.S1D # Deal with Sick1 Hazard Ratio
  rexp(1, rate)
}


years_till_healthy <- function(inputs)
# Identifies the state and returns the time to event
{
  state <- get_attribute(env, "State")
  if(state == 1) # Deal with Sick1 Hazard Ratio
  {rexp(1, inputs$r.S1H)
  }
  else{
    inputs$horizon+1 # Past end of simulation time
  }
}

sick1 <- function(traj, inputs)
# sets the state to sick1 and releases the healthy resource
{
  traj                      |> 
  set_attribute("State", 1) |> # 1 => Sick 1 (S1)
  release('healthy')        |> # Track state change for tally later
  seize('sick1')
}

healthy <- function(traj, inputs)
# sets the state to healthy and releases the sick1 resource
{
  traj                      |> 
  set_attribute("State", 0) |> # 0 => Healthy
  seize('healthy')          |>
  release('sick1')          # Track state change for tally later
}

# Given a trajectory, modify as needed when a secular
# death occurs.
#
# In this case, it marks a counter and terminates 
# the trajectory. A branch is required, even though
# it doesn't branch to force the termination.
death <- function(traj, inputs)
{
  traj |> branch(
    function() 1,
    continue=c(FALSE), # False is patient death, had to use a branch to force termination
    trajectory("Death") |>
      mark("death")     |> # Must be in 'counters'
      terminate_simulation()
  )
}