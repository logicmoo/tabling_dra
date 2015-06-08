
cid(exists,test) <= exists(rid(r2,test),cid(c12,test)).

cid('(exists,all)',test) <= 
               exists(rid(r2,test),cid(c12,test)),all(rid(r2,test),cid(c12,test)).

cid('exists(exists)',test) <= 
               exists(rid(r2,test),(cid(c23,test),exists(rid(r2,test),cid(c33,test)))).



