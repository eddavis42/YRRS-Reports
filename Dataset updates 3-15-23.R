year5 <- cbind(year5, nmMS2019YRRS_nm_v3$dental, nmMS2019YRRS_nm_v3$homeless, nmMS2019YRRS_nm_v3$concussion,
               nmMS2019YRRS_nm_v3$sleep8, nmMS2019YRRS_nm_v3$gamble)

year5 <- year5 %>% dplyr::rename(
  #New name = old name
  dental = 'nmMS2019YRRS_nm_v3$dental',
  homeless = 'nmMS2019YRRS_nm_v3$homeless',
  concussion = 'nmMS2019YRRS_nm_v3$concussion',
  sleep8 = 'nmMS2019YRRS_nm_v3$sleep8',
  gamble = 'nmMS2019YRRS_nm_v3$gamble'
)
write_dta(year5, "YRRS/County & District Reports/Datasets/nmMS_2019_Ed_v2.dta")

year5$sdid[(year5$dental==1)] == nmMS2019YRRS_nm_v3$sdid[(nmMS2019YRRS_nm_v3$dental==1)]

year5$psu[(year5$dental==0)] == nmMS2019YRRS_nm_v3$psu[(nmMS2019YRRS_nm_v3$dental==0)]



year4 <- cbind(year4, nmMS2017YRRS_nm_v2$dental, nmMS2017YRRS_nm_v2$concussion,
               nmMS2017YRRS_nm_v2$sleep8, nmMS2017YRRS_nm_v2$gamble)

year4 <- year4 %>% dplyr::rename(
  #New name = old name
  dental = 'nmMS2017YRRS_nm_v2$dental',
  concussion = 'nmMS2017YRRS_nm_v2$concussion',
  sleep8 = 'nmMS2017YRRS_nm_v2$sleep8',
  gamble = 'nmMS2017YRRS_nm_v2$gamble'
)

year4$sdid[(year4$dental==1)] == nmMS2017YRRS_nm_v2$sdid[(nmMS2017YRRS_nm_v2$dental==1)]

year4$psu[(year4$dental==1)] == nmMS2017YRRS_nm_v2$psu[(nmMS2017YRRS_nm_v2$dental==1)]

year4$fwt_str[(year4$sleep8==1)] == nmMS2017YRRS_nm_v2$fwt_str[(nmMS2017YRRS_nm_v2$sleep8==1)]

year4$fwt_str[(year4$gamble==1)] == nmMS2017YRRS_nm_v2$fwt_str[(nmMS2017YRRS_nm_v2$gamble==1)]

write_dta(year4, "YRRS/County & District Reports/Datasets/nmMS_2017_Ed_v2.dta")
