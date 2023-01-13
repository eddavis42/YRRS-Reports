#Use for district reports
svyby(~helmet, ~sdid==1, yrrs_nm, svyciprop, vartype ="ci", df = degf(yrrs_nm), na.rm=T)