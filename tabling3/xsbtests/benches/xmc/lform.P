fDef( ae_leader, lfp(fAnd( fDiamMinus( nil, tt),
                       fBoxMinus( leader, form(ae_leader))))).

fDef( ee_fail, lfp(fOr( fDiam( fail, tt),
                    fDiamMinus(nil, form(ee_fail))))).

fDef(aa_true, gfp(fAnd( fBox( fail, ff),
                fBoxMinus(nil, form(aa_true))))).
 
fDef(aa_leader, gfp(fAnd( fDiamMinus( nil, tt),
                       fBoxMinus( leader, form(aa_leader))))).


fDef( one_leader, lfp(fAnd( fBoxMinus(leader, form(one_leader)),
			    fBox(leader, form(no_leader))))).

fDef( no_leader, gfp(fAnd( fBox(leader, ff), 
			   fBoxMinus(nil, form(no_leader))))).


