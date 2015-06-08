% This file tests newExtTerm/1 and retractallExtTerm/1 for
% This checks reading and writing Intensional Rules.

:- [altcdf].
:- [test_utils].

:- import update_all_components/2 from cdf_comps_noshare.
:- import load_component/3  from cdf_comps_share.

create_second_copy :-
	load_component(testo1,comp_outdir,[]),
	update_all_components(comp_outdir1,[action(create)]).

end_of_file.

