
isa_int(X,Y):- isa_aux(X,Y).
hasAttr_int(X,Y,Z):- hasAttr_aux(X,Y,Z).
allAttr_int(X,Y,Z):- allAttr_aux(X,Y,Z).
coversAttr_int(X,Y,Z):- coversAttr_aux(X,Y,Z).
classHasAttr_int(X,Y,Z):- classHasAttr_aux(X,Y,Z).
minAttr_int(X,Y,Z,N):- minAttr_aux(X,Y,Z,N).
maxAttr_int(X,Y,Z,N):- maxAttr_aux(X,Y,Z,N).
%necessCond_int(X,Y):- necessCond_aux(X,Y).

