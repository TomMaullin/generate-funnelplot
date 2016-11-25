generateFunnelPlots <- function(mean, var, n, pubBias = FALSE){

	#n is number of studies.
	#k is max study size
	studySizes = c(rexp(floor(n/2), 0.45) + 1, rexp(ceiling(n/2), 0.1) + 1);
	studyObservedMeans = c();
	assigned = FALSE;
	for(i in 1:n){
		assigned = FALSE;
		while(assigned == FALSE){
			studyRecordings = rnorm(studySizes[i], mean, var);
			if(pubBias == FALSE || ((runif(1, 0, 1) < runif(1, 0, 0.25) || mean(studyRecordings) > mean-2+rnorm(1, 0, 1)))){
				studyObservedMeans[i] = mean(studyRecordings);
				assigned = TRUE;
			}
		}
	}
	z = qnorm(0.999, mean = 0, sd = 1, log = FALSE);
	standardErrors = var/sqrt(studySizes);
	counter = 0;
	for(i in 1:n){
		if(studyObservedMeans[i-counter] > (mean+z*standardErrors[i-counter]) || studyObservedMeans[i-counter] < mean-z*standardErrors[i-counter]){
			studyObservedMeans = studyObservedMeans[-(i-counter)];
			standardErrors = standardErrors[-(i-counter)];
			counter = counter+1;
		}
	}
	x = range(studyObservedMeans);
	y = range(standardErrors);
	plot(studyObservedMeans, standardErrors, xlim = c(x[1]-5, x[2]+5), ylim = rev(range(c(-1, y[2]))), , xlab = 'Observed effect', ylab = 'Standard Error', yaxs = "i", xaxs = "i");
	segments(mean, 0, mean+z*y[2], y[2])
	segments(mean, 0, mean-z*y[2], y[2])
}
generateFunnelPlots(7, 3, 900)