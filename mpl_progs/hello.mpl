var nTimes : int := 0;
print "How many times?" /**/;
read nTimes;
var x : int;
for x in 0..nTimes - 1 do
    print x;
    print ": Hello so called \"world!\"\n";
end for;
assert(x = nTimes);

var X : string := "false";
var a : int;
var asd : int := 10;
for a in 5 .. asd do
    asd := 5;
    print asd;
    print "\n";
end for;
