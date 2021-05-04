# Test giving file name directly vs via a function"

expect_identical('accident_2015.csv.bz2',make_filename(2015))
