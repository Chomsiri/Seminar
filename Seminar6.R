example_decision_inputs <- read.csv("example_decision_inputs.csv")

# Install packeage####
install.packages("decisionSupport")
library(decisionSupport)
install.packages("DiagrammeR")
library(DiagrammeR)

# example ####
example_decision_model <- function(x, varnames)
{
  profit <- benefits-costs
  
  final_profits <- profit + 500
  
  return(final_profits)
  
}

mcSimulation(estimate = as.estimate(example_decision_inputs),
             model_function = example_decision_model,
             numberOfModelRuns = 100,
             functionSyntax = "plainNames")

# Produce pathway analysis ####
#Run the code below and then add an additional cost to the graph called Management cost

mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        LC(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        DR(Depriciation Rate)-->F; linkStyle 4 stroke: red, stroke-width:1.5px")

mermaid("graph TB
        Y(Yield)-->I(Income); style I fill:green
        linkStyle 0 stroke:green, stroke-width:2px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:2px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:2px
        CL(Labor cost)-->F; style CL fill:red
        linkStyle 3 stroke: red, stroke-width:2px
        MC(Management cost)-->F; style MC fill:red
        linkStyle 4 stroke: red, stroke-width:2px
        DR(Depriciation Rate)-->F; style DR fill:red
        linkStyle 5 stroke: red, stroke-width:2px")

#Estimation ####

Management_cost <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Managment_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season",
                                              "Management costs in a normal season"))

Management_cost



# mcSimulation ####




model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Estimate the final results from the model
  final_result <- income - Labor_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

chile_mc_simulation

#plot distribution####

plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

#function to draw random variables of e.g. input data set to make a run of the model

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#draw random variables

make_variables(as.estimate(input_estimates))

Market_price

make_variables(as.estimate(input_estimates))

Management_cost

Labor_cost + Market_price
 
# Test ####

