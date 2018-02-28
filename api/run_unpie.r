#Used to run R locally
library('plumber')
r <- plumb('/var/unpie/api.R')
r$run(port=8888)
