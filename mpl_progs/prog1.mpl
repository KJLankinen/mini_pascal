var pi : int := 3;
var pie : string := "piira\"\nkka";

var i : int;

for i in pi .. pi * 5 do
    print "How many pies?" + pie;
    var n : int;
    read n;
    assert(1 < n);
    var j : int;
    for j in 1 .. n do
        print pie;
        print pi + (i + j);
    end for;
end for;

print "Job's done!";
