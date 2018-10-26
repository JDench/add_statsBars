# This is a function for adding significance bars around pairwise plot elements
# You pass the starting Y value where the bars should begin, an indication if
# the bars should be angled upward or downward from the start Y, and then pass the
# list of ranges for the two plot elements to be bounded.
# A user can specifically define the nextY (for the second grouping bar) and 
# the same for where the asterisk is placed, but you don't have to.
# You can pass other arguments that could be interpreted by lines or text function.
add_statsBars <- function(func_ranges, func_startY, func_nextY = NULL, func_asteriskY = NULL, 
							func_plotUp = TRUE, func_plotLabels = "*", ...){
	# We check that we've been passed a list of length two
	if(!is.list(func_ranges)){
		stopError("Passed a non list as func_ranges for add_statsBars, don't do this please.")
	}
	if(length(func_ranges) != 2){
		stopError("func_ranges needs a list of 2 elements, please pass that next time.")
	}
	# Ok, now we define the next position of the Y value if this has not been defined
	if(is.null(func_nextY) || !is.numeric(func_nextY)){
		func_nextY <- func_startY * ifelse(func_plotUp, 1.25,0.75)
	}
	# We plot the lines for the two ranges
	for(func_thisRange in func_ranges){
		# We plot the lines and pass other arguments in this space
		lines(x = func_thisRange,
				y = rep(func_startY,2),
				...)
		# And also the vertical line that will connect this to the above line
		lines(x = rep(mean(func_thisRange),2),
				y = c(func_startY,func_nextY),
				...)
	}
	# No we draw the additional bar between ranges
	lines(x = sapply(func_ranges,mean),
			y = rep(func_nextY,2),
			...)
	# Ok, now we define the asterisk position of the Y value if this has not been defined
	if(is.null(func_asteriskY) || !is.numeric(func_asteriskY)){
		func_asteriskY <- func_startY * ifelse(func_plotUp, 1.4,0.6)
	}
	text(x=mean(unlist(func_ranges)),
		y = func_asteriskY,
		labels = func_plotLabels,
		...)
	# We return nothing as this is a plotting function.
	return( NULL )
}

