var ntimes : int := 0;
print "How many times?";
read nTimes;
var x : int;
for x in 0..ntimes - 1 do
    print x;
    print ": Hello world!\n";
end for;
assert(x = nTimes);
