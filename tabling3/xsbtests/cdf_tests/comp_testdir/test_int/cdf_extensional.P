isa_ext(cid(component(cid(test_int,test_int),cid(allAtoms,cdf),cid(allIntegers,cdf)),test_int),cid(components,cdf)).
isa_ext(cid(compName(cid(test_int,test_int),cid(allAtoms,cdf)),test_int),cid(compNames,cdf)).
isa_ext(cid(test_int,test_int),cid(componentIDs,cdf)).
isa_ext(crid(hasVersion,test_int),crid('CDF Class-Object Relations',cdf)).
isa_ext(crid(componentDepends,test_int),crid('CDF Class-Object Relations',cdf)).
classHasAttr_ext(cid(test_int,test_int),crid(componentDepends,test_int),cid(component(cid(test,test),cid('.',cdfpt),cid(0,cdfpt)),test)).
classHasAttr_ext(cid(test_int,test_int),crid(componentDepends,test_int),cid(component(cid(cdf,cdf),cid('',cdfpt),cid(0,cdfpt)),cdf)).
classHasAttr_ext(cid(test_int,test_int),crid(hasVersion,test_int),cid(0,cdfpt)).
