## FUNCTIONS AVALIBLE ##

anova.R
	SST : function (x, nrow, ncol)  
	SSE : function (x, nrow, ncol)
	SSTr : function (x, nrow, ncol)
	MSTr : function (SSTr, k)	
	MSE : function (SSE, n, k)  
	anovaHyp : function (SSTr, SSE, n, k, alpha)  	postAnovaConf : function (y1, n1, y2, n2, n, k, MSE, alpha)  	postAnovaHyp : function (y1, n1, y2, n2, n, k, MSE, alpha)    bootStrapping.R	oneSampleBoot : function (set, k, func)  	twoSampleBoot : function (set1, set2, k, func)  
	oneSampleParaBoot : function (k, func, n, model, ...)  	twoSampleParaBoot : function (k, func, n1, n2, model, listArgs1, listArgs2)  confidenceIntervals.R	oneSampleConf : function (x, s, n, alpha)  	twoSampleConf : function (x1, s1, n1, x2, s2, n2, alpha)  hypothesisTest.R	oneSampleHyp : function (x, s, n, alpha, delta)  	twoSampleHyp : function (x1, s1, n1, x2, s2, n2, alpha, delta)  linearRegression.R	linReg : function (x, y)
	linStdErr : function (x, y)  
	linResiduals : function (x, y)  
	linStdErrBeta : function (x, y)	
	linHypBeta : function (beta, stdErrBeta, n, alpha, delta)  
	linConfBeta : function (beta, stdErrBeta, n, alpha)  	linConf : function (x, y, x0, alpha)  	linPredict : function (x, y, x0, alpha)  other.R	cor : function (set1, set2)  
	R2 : function (x, y)
	sigLevCovnert : function (p)	criticalValueT : function (alpha, df) 	vectorToMatrix : function (x, nrow, ncol)  proportions.R	onePropConf : function (n, phat, alpha)  	onePropHyp : function (x, n, p0, alpha)    	twoPropConf : function (n1, n2, phat1, phat2, alpha)  	twoPropHyp : function (x1, x2, n1, n2, alpha, df)  
	propMeanVar : function (p, n)

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