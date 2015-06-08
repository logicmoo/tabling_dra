% This file tests newExtTerm/1 and retractallExtTerm/1 for
% This checks reading and writing Intensional Rules.

:- [altcdf].
:- [test_utils].

:- import load_extensional_facts/1,
	 load_intensional_rules/1, dump_extensional_facts/1,
	 dump_intensional_rules/1 from cdf_io.
:- import rename/2 from shell.

testreturns:- 
	load_intensional_rules('io_testdir'),
	dump_intensional_rules(temp_testdir),
	rename('temp_testdir/cdf_intensional.P',ioInt_new).

end_of_file.

