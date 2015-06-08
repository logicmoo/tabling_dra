
isa_ext(cid(c13,test),cid('CDF Classes',cdf)).
isa_ext(cid(c11,test),cid(c12,test)).
isa_ext(cid(c12,test),cid(c13,test)).

isa_ext(cid(c23,test),cid('CDF Classes',cdf)).
isa_ext(oid(o23,test),cid(c23,test)).

isa_ext(oid(o33,test),cid(c33,test)).

isa_ext(oid(o11,test),cid(c11,test)).

hasAttr_ext(oid(o14,test),rid(r2,test),oid(o11,test)).

hasAttr_ext(oid(o15,test),rid(r2,test),oid(o11,test)).
allAttr_ext(oid(o15,test),rid(r2,test),oid(o11,test)).

hasAttr_ext(oid(o16,test),rid(r2,test),oid(o23,test)).
hasAttr_ext(oid(o23,test),rid(r2,test),oid(o33,test)).

% need to do products.

end_of_file.

