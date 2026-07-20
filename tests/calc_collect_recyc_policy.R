


policy = sr or policy = rc
bau_wastegen * bau_rr 

policy = rr 
bau_wastegen * rr_multiplier 

if (policy_type %in% c("sr", "rc")) {
  target_rr <- baseline_rate
}  
