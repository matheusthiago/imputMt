#package imput
#biblioteca para imputação de dados 
# library to impute data 
#require(imputeTS)
#require(forecast)

#library(Rssa)
#library(spectral.methods)
# plumber.R


#' função de imputação
#' Recebe o método de imputação escolhido, o dado com lacunas e o tamanho da lacuna
#' Retorna os a série temporal imputada 
#' @param method metodo a ser usado na imputação
#' @param dado dado com lacunas a serem imputadas
#' @param n tamanho da lacuna 
#' @export 
imputation <- function(method, dado, n){
	if (missing(method)){
    	print("You must specify a value for the 'method'")
  	}
	if (missing(dado)){
    	print("You must specify a value for the 'dado'")
  	}
	if (missing(method)){
    	print("You must specify a value for the 'n'")
  	}
  	print("noooovo")
	print(method)
	print(n)
	print(summary(dado))

	dadoImputado=c()
	freq=3600
	n=(as.numeric(n))

	switch(method,
		'linear'=(dadoImputado=interpolationImputation(dado, 1)),
    	'spline'=(dadoImputado=interpolationImputation(dado, 2)),
    	'stine'=(dadoImputado=interpolationImputation(dado, 3)),
	    'kalmanSmoothing'=(dadoImputado=kalman(dado, 1)),
	    'kalmanRun'=(dadoImputado=kalman(dado,2)),
	    'kalmanTS'=(dadoImputado=kalman(dado,3)),
	    'kalmanArima'=(dadoImputado=kalman(dado,4)),
	    'locf'=(dadoImputado=locfImputation(dado)),
	    'SMA'=(dadoImputado=movingAverageImputation(dado,1, n)),
	    'LMA'=(dadoImputado=movingAverageImputation(dado,2, n)),
	    'EMA'=(dadoImputado=movingAverageImputation(dado,3, n)),
	    'ssa1'=(dadoImputado=ssaImputation(dado, n, 1, 1)),
		'ssa'=(dadoImputado=ssaImputation(dado, n, 2, 1)),
	    'mean'=(dadoImputado=meanImputation(dado, 1)),
	    'median'=(dadoImputado=meanImputation(dado, 2)),
	    'mode'=(dadoImputado=meanImputation(dado, 3)),
   	    'seadec'=(dadoImputado=seaImputation(dado,1,freq)),
   	    'seasplit'=(dadoImputado=seaImputation(dado,2,freq)),
   	    'interp'=(dadoImputado=interpImputation(dado,freq))
		)
	print("----------inicio--------")

	print(summary(dadoImputado))

	print("----------fim--------")

	return(dadoImputado)	
}



#' Recebe algo
#' retorna algo
#' @param dado dado qualquer
#' @return dado 

printDado<-function(dado){
	return(dado)
}


interpImputation=function(dadoTemp, freq){
	dadoTemp <- ts(dadoTemp, frequency = freq)
	dadoImputado=imputeTS::na_interp(dadoTemp)
	return(dadoImputado)
}

seaImputation = function(dadoTemp, type, freq){
	dadoTemp <- ts(dadoTemp, frequency = freq)
	if(type==1){
		dadoImputado=imputeTS::na_seadec(dadoTemp)
	}else if (type==2){
		dadoImputado=imputeTS::na_seasplit(dadoTemp)
	}
	return(dadoImputado)
}

#Uses Kalman Smoothing on structural time series models (or on the state space representation of an arima model) for imputation

kalman= function(dadoTemp, type, freq){
	dadoTemp <- ts(dadoTemp, frequency = 6)

	if(type==1){
		#: Perform imputation with KalmanSmoother and state space representation of arima model
		dadoImputado=imputeTS::na_kalman(dadoTemp)
	}
	else if(type==2){
		# Example 2: Perform imputation with KalmanRun and state space representation of arima model
		dadoImputado=imputeTS::na_kalman(dadoTemp, smooth=FALSE)
	}
	else if(type==3){
		#Perform imputation with KalmanSmooth and StructTS model;
		dadoImputado=imputeTS::na_kalman(dadoTemp, model="StructTS", smooth=TRUE)	
	}
	else if(type==4){
		dadoImputado=imputeTS::na_kalman(dadoTemp, model="auto.arima")
	}
	return(dadoImputado)
}


#Missing Value Imputation by Interpolation
#Uses either linear, spline or stineman interpolation to replace missing values.
#Usage na_interpolation(x, option = "linear", maxgap = Inf, ...
#• "linear" - for linear interpolation using approx 
#• "spline" - for spline interpolation using spline 
#• "stine" - for Stineman interpolation using stinterp
interpolationImputation = function(dadoTemp, type) {
	print("passou")
	print(type)
	if(type==1){
		dadoImputado=imputeTS::na_interpolation(dadoTemp)
	}else if(type==2){
		dadoTemp=ts(dadoTemp, frequency=3600)
		dadoImputado=imputeTS::na_interpolation(dadoTemp, option ="spline")
	}else if(type==3){
		dadoImputado=imputeTS::na_interpolation(dadoTemp, option ="stine")
	}
	return(dadoImputado)

}

#Missing Value Imputation by Last Observation Carried Forward
#Replaces each missing value with the most recent present value prior to it (Last Observation Carried Forward- LOCF). 
#Optionally this can also be done starting from the back of the series (Next Observation Carried Backward - NOCB).

locfImputation = function (dadoTemp){
    dadoImputado=imputeTS::na_locf(dadoTemp)
}



#na_ma Missing Value Imputation by Weighted Moving Average
#Missing value replacement by weighted moving average. Uses semi-adaptive window size to ensure all NAs are replaced.
#na_ma(x, k = 4, weighting = "exponential", maxgap = Inf)
#integer width of the moving average window. Expands to both sides of the center
#element e.g. k=2 means 4 observations (2 left, 2 right) are taken into account. If
#increased until there are at least 2 non-NA values present.

#weighting Weighting to be used. Accepts the following input:
#• "simple" - Simple Moving Average (SMA) - all observations in the window are equally weighted for calculating the mean.

#• "linear" - Linear Weighted Moving Average (LWMA) - weights decrease in arithmetical progression. 
#The observations directly next to a central value i, have weight 1/2, the observations one further away (i-2,i+2) have weight 1/3, the next (i-3,i+3) have weight 1/4,

#• "exponential" - Exponential Weighted Moving Average (EWMA) - : uses weighting factors which decrease exponentially. 
#The observations directly next to a central value i, have weight 1/2^1, the observations one further away (i-2,i+2) have weight 1/2^2 ...

movingAverageImputation = function(dadoTemp, type, n){
	if(type==1){
		dadoImputado=imputeTS::na_ma(dadoTemp, weighting="simple", k=n)
	}else if(type==2){
		dadoImputado=imputeTS::na_ma(dadoTemp, weighting="linear", k=n)
	}else if(type==3){
		dadoImputado=imputeTS::na_ma(dadoTemp, weighting="exponential", k=n)
	}
}
#Missing value replacement by mean values. Different means like median, mean, mode possible.
meanImputation = function(dadoTemp, type){
	if(type==1){
		dadoImputado=imputeTS::na_mean(dadoTemp)
	} else if (type==2){
		dadoImputado=imputeTS::na_mean(dadoTemp, option="median")
	}else if(type==3){
		dadoImputado=imputeTS::na_mean(dadoTemp, option="mode")
	}
}



ssaImputation = function(dadoTemp, n, method, type){
	print("-------entrou no SSA------------")
	if(method==1){
		s=Rssa::ssa(dadoTemp, n)
		dadoImputado=Rssa::igapfill(s, groups=list(1:6))
		}
	else if(method==2){
		if(n<=10){
			g=1
			n=10
		}else if(n<=100){
			g=6
		}else if(n>100){
			g=95
		}
		if(type==1){ #Consecutivo
			s=Rssa::ssa(dadoTemp, n)
			dadoImputado=Rssa::gapfill(s, groups=list(1:g))		
		}
		else if(type==2){ #Aleatório
			n=10
			g=2
			s=Rssa::ssa(dadoTemp, n)
			dadoImputado=Rssa::gapfill(s, groups=list(1:g))
		}
		}
	return(dadoImputado)
}
