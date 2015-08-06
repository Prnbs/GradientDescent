var line = "Finally if you are thinking of using synchronized collections you may also wish to consider the concurrent collections of java util concurrent instead."
var first = line.split(" ")
var second = first.tail
var third = second.tail
var tri = (first zip second zip third)
var fram = tri..map{((x,y),z) => (x, y, z)}








//var third = second.tail
