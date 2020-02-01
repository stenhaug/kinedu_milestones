fix_factors <- function(x){
	out <- NA
	if(x == "phys"){
		out <-
			mirt.model(
				glue::glue(
					"Phys = {paste(which(item_area == 'Physical'), collapse = ',')}
                NonPhys = {paste(which(item_area != 'Physical'), collapse = ',')}
                COV = Phys*NonPhys"
				)
			)
	}
	else if(x == "phys_ling"){
		out <-
			mirt.model(
				glue::glue(
					"Phys = {paste(which(item_area == 'Physical'), collapse = ',')}
                Ling = {paste(which(item_area == 'Linguistic'), collapse = ',')}
                Other = {paste(which(!item_area %in% c('Physical', 'Linguistic')), collapse = ',')}"
				)
			)
	}
	else if(x == "interaction"){
		out <-
			mirt.model(
				"F1 = 1-414
                 F2 = 1-414
                 (F1*F2) = 1-414"
			)
	}
	else{
		out <- as.numeric(x)
	}
	out
}
