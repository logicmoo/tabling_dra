% This file tests newExtTerm/1 and retractallExtTerm/1 for
% This checks reading and writing Intensional Rules.

:- [altcdf].
:- [test_utils].

:- import load_extensional_facts/1,
	 load_intensional_rules/1, dump_extensional_facts/1,
	 dump_intensional_rules/1 from cdf_io.

:- import update_all_components/2 from cdf_comps_noshare.

testreturns:- 
	load_extensional_facts('comp_testdir'),
	load_intensional_rules('comp_testdir'),
	load_dyn('comp_testdir/initialization_file.P'),
	update_all_components(comp_outdir,[action(create)]).

end_of_file.

