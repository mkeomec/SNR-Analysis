# Creates list of HASPI SNR 8 estimate to fill Basecamp database

setwd("H:/SNR Subject Data/Project AD_ Hearing Aids and SNR/SIN/subject_data/Results/master dataset")

current_file_data <- read.csv('Master_HASPI_Aided_requestedSNR8 ,2018-09-27.csv')

HASPI_validation <- c(1010,1012,1013,1014,1015,1016,1017,1018,1019,1020,1021,1022,1024,1025,1026,1027,1028,1029,1030,1032,1033,1034,1035,1036,1037,1038,1039,1040,1041,1042,1043,1044,1045,1046,1047,1048,1050,1051,1052,1053,1054,1055,1056,1057,1058,1059,1060,1061,1062,1063,1064,1065,1067,1068,1069,1070,1071,1072,1073,1074,1075,1076,1077,1078,1079,1080,1081,1082,1083,1084,1085,1086,1087,1088,1089,1090,1091,1092,1093,1094,1095,1096,1097,1098,1099,1100,1101,1102,1103,1104,1105,1106,1107,1108,1111,2011,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035,2036,2037,2038,2039,2040,2041,2042,2043,2044,2045,2046,2047,2048,2049,2050,2051,2052,2053,2054,2055,2056,2057,2058,2059,2060,2061,2062,2063,2064,2065,2066,2067,2068,2069,2070,2071,2072,2073,2074,2075,2076,2077,2078,2079,2080,2081,2082,2083,2084,2085,2086,2087,2088,2089,2090,2091,2092,2093)

current_file_data[current_file_data$SubId%in%HASPI_validation]

filtered_data <- filter(current_file_data,SubId%in%HASPI_validation)
filtered_data <- filter(current_file_data,SNR_requested==0)
filtered_ISTS <- filter(filtered_data,Noise_type=='ISTS')
filtered_SPSHN <- filter(filtered_data,Noise_type=='SPSHN')

filtered_ISTS_1 <- filter(filtered_ISTS,Ear.1.left.==1)
filtered_ISTS_2 <- filter(filtered_ISTS,Ear.1.left.==2)
filtered_SPSHN_1 <- filter(filtered_SPSHN,Ear.1.left.==1)
filtered_SPSHN_2 <- filter(filtered_SPSHN,Ear.1.left.==2)

filtered_ISTS_1[,c(1,8)]
filtered_ISTS_2[,c(1,8)]
filtered_SPSHN_1[,c(1,8)]
filtered_SPSHN_2[,c(1,8)]

#copied and pasted into excel to remove numeric leftside column, then pasted into basecamp database