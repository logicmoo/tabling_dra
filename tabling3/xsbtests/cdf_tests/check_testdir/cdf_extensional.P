isa_ext(cid(c3,test),cid('CDF Classes',cdf)).
isa_ext(cid(c1,test),cid(c2,test)).
isa_ext(cid(c2,test),cid(c3,test)).
isa_ext(oid(o12,test),cid(c12,test)).

hasAttr_ext(oid(o2,test),rid(r4,test),cid(c12,test)).
hasAttr_ext(oid(o2,test),rid(r4,test),oid(o12,test)).

hasAttr_ext(cid(c2,test),rid(prod(rid(r2,test),rid(r2,test)),test),cid(c12,test)).

