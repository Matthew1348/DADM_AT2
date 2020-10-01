#Monte carlo simulation for estimated price for the cloud solution
#triangular distributions

rm(list=ls())

inv_triangle_cdf <- function(P, vmin, vml, vmax){

  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}

#no of simulation trials
n=10000

#read in cost data

solution_costs <- read.csv(file="solution_costs.csv", stringsAsFactors = F)
str(solution_costs)

#set seed for reproducibility
set.seed(42)

#create data frame with rows = number of trials and cols = number of tasks
csim <- as.data.frame(matrix(nrow=n,ncol=nrow(solution_costs)))

# for each task
for (i in 1:nrow(solution_costs)){
  #set task costs
  vmin <- solution_costs$cmin[i]
  vml <- solution_costs$cml[i]
  vmax <- solution_costs$cmax[i]
  
  #generate n random numbers (one per trial)
  psim <- runif(n)
  #simulate n instances of task
  csim[,i] <- inv_triangle_cdf(psim,vmin,vml,vmax) 
}

#sum costs for each trial
price <- csim[,1] + csim[,2] + csim[,3] + csim[,4] #costs add
price

#plot histogram
hist(price,
     main="Estimated price for Cloud Solution",
     xlab="price ('000)",
     xlim=c(650,1250),
     col="yellow"
)

#mean, max, min and median cost
mean(price)
max(price)
min(price)
median(price)

#standard deviation
sd(price)

#plot cdf
plot(ecdf(price), col="red")

# costs corresponding to 5, 50 and 95% chance
quantile(ecdf(price),c(0.05,0.5,0.95),type=7)

quantile(ecdf(price),.59,type=7)





