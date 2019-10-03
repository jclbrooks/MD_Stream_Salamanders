COPY(SELECT featureid, date_part('year', date) as year, date, tmax, tmin, prcp, dayl, srad, swe FROM daymet WHERE featureid IN (205398689, 205399857, 205401164, 205400902, 205400805, 2021047603, 2022348265, 2021049849, 2021047761, 2021049850, 2021051806, 2021047824, 2021049066, 2021047689, 2021070538, 2021059967, 2021059671, 2021058819, 2021059130, 2021059761, 2021059719, 2021059995, 2021059295, 2021058743, 2021049317, 2021058586, 2021049618, 2021050021, 2021049984, 2021058897, 2021058896, 2021059874, 2021060032, 2021064768, 2021065452, 2021065382, 2021062818, 2021062408, 2021061379, 2021071416, 2021074305, 2021081056, 2021081491, 2021081654, 2021074364, 2021073775, 2021071311, 2021064790, 2021064784, 2021062953, 2021062407, 2021074306, 2021064525, 2021064767, 2021065189, 2021062652, 2021062817, 2021073980, 2021041478, 205386440, 2021041507, 2021042707, 2021040567, 205386772, 205388232, 205387301, 205387966) AND date_part('year', date) IN (2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) ) TO STDOUT CSV HEADER;