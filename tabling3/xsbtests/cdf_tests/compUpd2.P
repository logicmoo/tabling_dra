% This file tests newExtTerm/1 and retractallExtTerm/1 for
% This checks reading and writing Intensional Rules.

:- [altcdf].
:- [test_utils].

:- import update_all_components/2 from cdf_comps_noshare.
:- import load_component/3  from cdf_comps_share.

testreturns:- 
	load_component(testo1,comp_outdir,[]),
	assert(cdf_flags(dirty,test)),
	update_all_components(comp_outdir,[action(in_place)]).

end_of_file.

