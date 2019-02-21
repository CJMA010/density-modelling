# density-modelling

It's a ideal way to use butterfly spread to model probability density of the price of the underlying at expirtion. In this project, the following procedures are implemented to realize the above statement.

## Obtain bar plot for densities:

	Obtain option strike price for everyday we estimate
	Find the max and min value of the put and call strike
	Divide the range (min, max) into sub intervals with length=5
	For each strike price K=min+n*5, obtain its settlement price, (if there is no data, return NA)
	Using diff(diff(selltemet.call))/5 and diff(diff(selltemet.put))/5 to construct price for butterfly
	Average butterfly spread prices for call and put at the same strike price, divide the price by 5
	Normalize the return and density, the bar plot are obtained
  
## Find parameters for normal distributions to examin:

	Use μ=0,σ=σ_(t-20:t) (T-t) to plot a normal density on the bar plot
