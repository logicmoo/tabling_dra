isa_aux(cid(c3,test),cid('CDF Classes',cdf)).
isa_aux(cid(c13,test),cid('CDF Classes',cdf)).
isa_aux(cid(c1,test),cid(c2,test)).
isa_aux(cid(c2,test),cid(c3,test)).

isa_aux(cid(f(a,X),cdfpt),cid(c11,test)).
isa_aux(cid(c11,test),cid(c12,test)).
isa_aux(cid(c12,test),cid(c13,test)).

isa_aux(rid(r1,test),rid(r2,test)).
isa_aux(rid(r2,test),rid(r3,test)).
isa_aux(rid(r3,test),rid('CDF Object-Object Relations',cdf)).
isa_aux(rid(r4,test),rid('CDF Object-Object Relations',cdf)).
isa_aux(cid(f(cid(c1,test),
                  cid(g(cid(c1,cdf),cid(c2,test)),
	             test)),
	         test),
    	     cid(c13,test)).

isa_aux(crid(s1,test),crid(s2,test)).
isa_aux(crid(s2,test),crid(s2,cdf)).
isa_aux(crid(s2,test),crid(s3,test)).
isa_aux(crid(s3,test),crid('CDF Class-Object Relations',cdf)).

isa_aux(oid(o1,testo1),cid(c1,test)).
isa_aux(oid(o2,test),cid(c2,test)).
isa_aux(oid(o3,test),cid(c3,test)).
isa_aux(oid(o11,test),cid(c11,test)).
isa_aux(oid(o12,test),cid(c12,test)).
isa_aux(oid(o13,test),cid(c13,test)).

isa_aux(rid(prod(rid(r2,test),rid(r2,test)),test),
	rid('CDF Object-Object Relations',cdf)).

isa_aux(crid(prod(crid(s2,test),crid(s2,test)),test),
	crid('CDF Class-Object Relations',cdf)).

hasAttr_aux(cid(c2,test),rid(r2,test),cid(c12,test)).
hasAttr_aux(cid(c2,test),rid(r2,test),oid(o12,test)).
hasAttr_aux(oid(o2,test),rid(r1,test),cid(c12,test)).
hasAttr_aux(oid(o2,test),rid(r4,test),cid(c12,test)).
hasAttr_aux(oid(o2,test),rid(r2,test),oid(o12,test)).
hasAttr_aux(oid(o2,test),rid(r4,test),oid(o12,test)).

hasAttr_aux(cid(c2,test),rid(prod(rid(r2,test),rid(r2,test)),test),cid(c12,test)).

classHasAttr_aux(cid(c2,test),crid(s2,test),cid(c12,test)).
classHasAttr_aux(cid(c2,test),crid(s2,test),oid(o12,test)).

classHasAttr_aux(crid(s2,test),crcrid('Name',test),oid('s2 cridname',test)).
classHasAttr_aux(rid(r2,test),crrid(s2,test),oid('rd ridname',test)).

classHasAttr_aux(cid(c2,test),
	              crid(prod(crid(s2,test),crid(s2,test)),test),cid(c12,test)).

allAttr_aux(cid(c2,test),rid(r2,test),cid(c12,test)).
allAttr_aux(cid(c2,test),rid(r2,test),oid(o12,test)).
allAttr_aux(oid(o2,test),rid(r1,test),cid(c12,test)).
allAttr_aux(oid(o2,test),rid(r4,test),cid(c12,test)).
allAttr_aux(oid(o2,test),rid(r2,test),oid(o12,test)).
allAttr_aux(oid(o2,test),rid(r4,test),oid(o12,test)).

allAttr_aux(cid(c2,test),rid(prod(rid(r2,test),rid(r2,test)),test),cid(c12,test)).

minAttr_aux(cid(c2,test),rid(r2,test),cid(c12,test),2).
minAttr_aux(cid(c2,test),rid(r2,test),oid(o12,test),2).
minAttr_aux(oid(o2,test),rid(r1,test),cid(c12,test),2).
minAttr_aux(oid(o2,test),rid(r4,test),cid(c12,test),2).
minAttr_aux(oid(o2,test),rid(r2,test),oid(o12,test),2).
%minAttr_aux(oid(o2,test),rid(r4,test),oid(o12,test),2).

minAttr_aux(cid(c2,test),rid(prod(rid(r2,test),rid(r2,test)),test),cid(c12,test),2).

maxAttr_aux(cid(c2,test),rid(r2,test),cid(c12,test),2).
maxAttr_aux(cid(c2,test),rid(r2,test),oid(o12,test),2).
maxAttr_aux(oid(o2,test),rid(r1,test),cid(c12,test),2).
maxAttr_aux(oid(o2,test),rid(r4,test),cid(c12,test),2).
maxAttr_aux(oid(o2,test),rid(r2,test),oid(o12,test),2).
maxAttr_aux(oid(o2,test),rid(r4,test),oid(o12,test),2).

maxAttr_aux(cid(c2,test),rid(prod(rid(r2,test),rid(r2,test)),test),
	                                                         cid(c12,test),2).

coversAttr_aux(cid(c2,test),rid(r2,test),cid(c12,test)).
coversAttr_aux(cid(c2,test),rid(r2,test),cid(c13,test)).
coversAttr_aux(cid(c2,test),rid(r3,test),cid(c12,test)).

end_of_file.

:- writeln('loading init file for test').

r(_,_):- fail.
p(_,_):- fail.
q(_,_):- fail.

classHasAttr_int(X,cid(r_int,test),Y):- p(X,Y).
classHasAttr_int(X,cid(r_int,testo1),Y):- q(X,Y).
classHasAttr_int(cid(X,test),cid(r_int,test_int),cid(Y,test)):- r(X,Y).

auxiliary_cdf_rule(test,r(X,Y),fail).
auxiliary_cdf_rule(test,p(X,Y),fail).
auxiliary_cdf_rule(test,q(X,Y),fail).


