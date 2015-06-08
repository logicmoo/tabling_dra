:- index(t/3,[2+1],17).
:- index(u/3,[3+2+1,2+1,3+2,1,2,3],17).

test5 :- abolish(t/3),
	assert(t(a,b,c)),
	assert(t(a,c,d)),
	assert(t(a,d,e)),
	assert(t(a,d,f)),
	assert(t(a,d,g)),
	assert(t(a,g,h)),
	assert(t(a,h,i)),
	assert(t(b,h,i)),
	assert(t(b,j,k)),
	findall(X,t(a,d,X),[e,f,g]),
	findall(X,t(a,b,X),[c]),
	findall(X,t(b,h,X),[i]),
	findall(t(X,Y),t(a,X,Y),
		[t(b,c),t(c,d),t(d,e),t(d,f),t(d,g),t(g,h),t(h,i)]),
	retractall(t(a,d,X)),	
	findall(t(X,Y),t(a,X,Y),
		[t(b,c),t(c,d),t(g,h),t(h,i)]),
	retractall(t(X,h,Y)),
	findall(t(X,Y,Z),t(X,Y,Z),[t(a,b,c),t(a,c,d),t(a,g,h),t(b,j,k)]),
	retractall(t(b,j,_)),
	findall(t(X,Y,Z),t(X,Y,Z),[t(a,b,c),t(a,c,d),t(a,g,h)]),
	retractall(t(a,_,_)),findall(X,t(X,Y,Z),[]),
	true.

test6 :-
	assert(u(a,b,c)),
	assert(u(a,c,d)),
	assert(u(a,d,e)),
	assert(u(a,d,f)),
	assert(u(a,d,g)),
	assert(u(a,g,h)),
	assert(u(a,h,i)),
	assert(u(b,h,i)),
	assert(u(b,j,k)),
	findall(X,u(a,d,X),[e,f,g]),
	findall(X,u(a,b,X),[c]),
	findall(X,u(b,h,X),[i]),
	findall(con,u(a,d,e),[con]),
	findall(u(X,Y),u(a,X,Y),
		[u(b,c),u(c,d),u(d,e),u(d,f),u(d,g),u(g,h),u(h,i)]),
	retractall(u(a,d,X)),	
	findall(u(X,Y),u(a,X,Y),
		[u(b,c),u(c,d),u(g,h),u(h,i)]),
	retractall(u(X,h,Y)),
	findall(u(X,Y,Z),u(X,Y,Z),[u(a,b,c),u(a,c,d),u(a,g,h),u(b,j,k)]),
	retractall(u(b,j,_)),
	findall(u(X,Y,Z),u(X,Y,Z),[u(a,b,c),u(a,c,d),u(a,g,h)]),
	retractall(u(a,_,_)),findall(X,u(X,Y,Z),[]),
	abolish(u/3),


	true.
test7 :-
	true.
