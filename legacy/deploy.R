
install.packages("rsconnect")

rsconnect::setAccountInfo(
  name = "wilsonfreitas",
  token = "4276BE10F0B4A629C23F76219EEED085",
  secret = "voXmb5K5Urfn85F0H1DSfyghDyXutLtkzxdIwMsa"
)

library(rsconnect)
rsconnect::deployApp(".")