library(plumber)

# load script with api methods
r <- plumb("api_methods.r")

# define port assigned to group
GROUP_NUMBER = 20

# THIS WILL BE AVAILABLE AT http://p01.piterdata.ninja
r$run(port=8100 + GROUP_NUMBER)
