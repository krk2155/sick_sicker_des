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

sick1 <- function(traj, inputs)
{
  traj                      |> 
  set_attribute("State", 1) |> # 1 => Sick 1 (S1)
  release('healthy')        |> # Track state change for tally later
  seize('sick1')            |>
  branch( 
    function() get_attribute(env, "Treat")+1,
    continue = rep(TRUE, 2),
    trajectory(),  # No Treatment
    trajectory() |> seize('treat')
  )
}
# do I need to add the below code to the sick1 in order for 
# "Treat" attribute to keep changing? 
#  |>
#   set_attribute("Treat", TRUE)