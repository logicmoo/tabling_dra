% This file tests newExtTerm/1 and retractallExtTerm/1 for
% This checks reading and writing Intensional Rules.

:- [altcdf].
:- [test_utils].

:- import load_extensional_facts/1,
	 load_intensional_rules/1, dump_extensional_facts/1,
	 dump_intensional_rules/1 from cdf_io.
:- import rename/2 from shell.

testreturns:- 
	load_extensional_facts('io_testdir'),
	dump_extensional_facts(temp_testdir),
	rename('temp_testdir/cdf_extensional.P',temp).

end_of_file.

