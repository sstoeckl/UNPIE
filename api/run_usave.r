#Used to run R locally
library('plumber')
r <- plumb('api.R')
r <- plumb('api/api.R')
r$run(host="127.0.0.1", port=8000, swagger=TRUE)
