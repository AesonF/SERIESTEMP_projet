rm(list = ls(all.names = TRUE))
#install.packages("plotly")
#install.packages("forecast")
#install.packages("itsmr")
#install.packages("portes")
library(plotly)
library(forecast)
library(itsmr)
library(portes)
address = "/Users/aesonfeehan/Documents/SCHOOL/ENSAE2/SERIESTEMP/series.csv"
series = read.csv(address, sep=';')



#QUESTION 2 :
#  YEARLY DIFFERENCING:
#  The idea is to subtract values 12 months apart to get rid of seasonal effects on ice-cream
#  production. It sort of works.
series2 <- series
naive_ds <- function(vector, s){ #s=12 in this case
	out = vector
	l = length(vector)
	for(k in 1:s){
		out[k] = 0
	}
	for(k in (s+1):l){
		out[k] = vector[k]-vector[k-s]
	}
	return(out)
}
#  variance is not constant, so we be loggin'
series2[["indice"]] <- naive_ds( log(series2[["indice"]]) , 12)
series2 <- tail(series2,200)


#  Making sure that the series is stationary-ish:
series2[["numDate"]] <- as.numeric(series2[["date"]])
stationariness_reg <- lm(indice~numDate, data = series2)
summary(stationariness_reg)
#  the best linear approximation of the data is the X axis, so we're all done here.



#QUESTION 3 :
plot_series <- function(series){
	smean = mean(series[["indice"]])
	ssd = sd(series[["indice"]])
	fig <- plot_ly(x=series[["date"]], y=series[["indice"]], type='scatter', mode = 'lines',
				   name="production index")
	fig <- fig %>% add_lines(y=smean,line=list(color="black"), name = "mean")
	fig <- fig %>% add_lines(y=1.96*ssd+smean,
							 line=list(color="rgb(200,200,200)", dash="dash"),
							 name = "mean + 1.96*std")
	fig <- fig %>% add_lines(y=-1.96*ssd+smean,
							 line=list(color="rgb(200,200,200)", dash="dash"),
							 name = "mean - 1,96*std")
	fig
}

plot_series(series=series)
plot_series(series= series2)
#  variance is obviously not constant: we could try transforming the data again...



#QUESTION 4 : ARMA(p,q)
#  we're keeping series2, which is seasonally adjusted through differencing:
acf(series2[["indice"]], lag.max=36, plot=TRUE)
#  we can also look at the initial series, but it isn't very interesting.
#acf(series[["indice"]], lag.max=48, plot=TRUE)
# same for determining q
pacf(series2[["indice"]], lag.max=36, plot=TRUE)
#  p=75, q=50 (that's a lot, but heck...)
#  ACF and PACF tail off and don't cut off, meaning that ARMA is more appropriate
#  than AR or MA.

arma(as.vector(series2[["indice"]]), p=3, q=1)
#  AICC = -904.4891   yowza!
arma(as.vector(series2[["indice"]]), p=4, q=1)
#  AICC = -908.7273 and much nicer coefficients
#  okay this is just boring



ILikeBigParams <- function(vec, pmax, qmax){ #now we're talkin'!
	par = c(1,1)
	aicc0 = as.numeric(arma(vec, p=par[1], q=par[2])["aicc"])
	for(i in 1:pmax){
		for(j in 1:qmax){
			aicc = as.numeric(arma(vec, p=i, q=j)["aicc"])
			if(aicc < aicc0){
				par = c(i,j)
				aicc0 = aicc
			}
		}
	}
	print(par[1])
	print(par[2])
	print(aicc0)
}
ILikeBigParams(series2[["indice"]], 5, 10)
#  for (p,q)=(5,5), we get AIC= -947. It's pretty difficult to get anything
#  much better, even by exploring around (1:10)*(1:10).
goodArma <- arma(as.vector(series2[["indice"]]), p=4, q=1)
goodArma
BoxPierce(obj=goodArma)
#I HATE R, R CAN BURN IN THE FIREY DEPTHS OF HELL BAAAAAAAAAAAAAAAAAAAAAAAAA

#=====================================================================================#
#======================= EXTRA STUFF: LOESS SEASONAL ADJUSTMENT ======================#
#=====================================================================================#

vals <- ts(series[["indice"]], frequency=12)
decomp <- stl(vals, s.window = "periodic")
ap.sa <- seasadj(decomp)
sadj <- cbind(series[["indice"]], SeasonallyAdjusted=ap.sa) #seasonally adjusted series
series3 <- series
series3[["indice"]] <- sadj[,2]