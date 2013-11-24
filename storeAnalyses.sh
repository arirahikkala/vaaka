perl -pe"s/\D*(\d+)\D*(\d+),(\d+)\D*(\d+),(\d+)\D*(\d+),(\d+).*/update loads set dryStuff = 100 - \2.\3, density = \4.\5, protein = \6.\7 where sampleNum = \1;/" < analyysit.txt | sqlite3 vaaka/vaaka.db

