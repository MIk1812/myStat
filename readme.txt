This is a library of functions meant to compliment the course 02323 Introduction to statistics. Note: the functions are not intended to replace other R functions but are to be used in conjunction with them.

Use at your own risk. Most functions have been tested but not all.

## INSTALLTION ##

1. The following libraries need to be installed in R:

- png
- grid
- gridExtra

This can be done using install.packages(c("png", "grid", "gridExtra"))

2. Place the project somewhere you like on your computer
3. Open myStat.R at the project root
4. Update the variable ROOT and SEPERATOR to reflect the projects current location on the machine

You can now import all the functions in a new R-script using source(), for instance:
source("/Users/mikkeldanielsen/myStat/myStat.R")

## FUNCTIONS AVALIBLE ##

confidenceIntervals.R	oneSampleConf : function (x, s, n, alpha)  	twoSampleConf : function (x1, s1, n1, x2, s2, n2, alpha)  hypothesisTest.R	oneSampleHyp : function (x, s, n, alpha, delta)  	twoSampleHyp : function (x1, s1, n1, x2, s2, n2, alpha, delta)bootStrapping.R	oneSampleBoot : function (set, k, func)  	twoSampleBoot : function (set1, set2, k, func)  
	oneSampleParaBoot : function (k, func, n, model, ...)  	twoSampleParaBoot : function (k, func, n1, n2, model, listArgs1, listArgs2)    linearRegression.R	linReg : function (x, y)
	linStdErr : function (x, y)  
	linResiduals : function (x, y)  
	linStdErrBeta : function (x, y)	
	linHypBeta : function (beta, stdErrBeta, n, alpha, delta)  
	linConfBeta : function (beta, stdErrBeta, n, alpha)  	linConf : function (x, y, x0, alpha)  	linPredict : function (x, y, x0, alpha)  proportions.R	oneSamplePropConf : function (n, phat, alpha)  	oneSamplePropHyp : function (x, n, p0, alpha)    	twoSamplePropConf : function (n1, n2, phat1, phat2, alpha)  	twoSamplePropHyp : function (x1, x2, n1, n2, alpha)  
	propMeanVar : function (p, n)

anova.R
	SST : function (x, nrow, ncol)  
	SSE : function (x, nrow, ncol)
	SSTr : function (x, nrow, ncol)
	MSTr : function (SSTr, k)	
	MSE : function (SSE, n, k)  
	anovaHyp : function (SSTr, SSE, n, k, alpha)  
	postAnovaHyp : function (y1, n1, y2, n2, n, k, MSE, alpha)    	postAnovaConf : function (y1, n1, y2, n2, n, k, MSE, alpha)  

sampleSize.R
	oneSampleSizeConf : function(ME, sigma, alpha)
	oneSampleSizePower : function(mu0, mu1, sigma, alpha, beta)
	onePropSizeConf : function(p, ME, alpha)
		
other.R	cor : function (set1, set2)  
	R2 : function (x, y)
	sigLevCovnert : function (p)	criticalValueT : function (alpha, df)
	criticalValueChi : function (alpha, df)
	criticalValueF : function (alpha, n, k) 	vectorToMatrix : function (x, nrow, ncol)  

info.R
	iprobConvert : function (...)
	icalcRules : function (...)
	idists : function (...)
	iexpDist : function (...)
	ilogNormDist : function (...)
	inormDist : function (...)
	iuniDist : function (...)
	ipoisDist : function (...)
	ihypgeoDist : function (...)
	ibinomDist : function (...)