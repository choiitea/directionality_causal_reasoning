# Causal Training with both RS and GCy events in one example

name: "Cause on Base (ShapeONE)"
freq: 0.5
2
# 2 events: RS event and GCy event
[0]
I:
(ShapeONE)
1 0 0 0 0 0 0
T:
(ShapeONEOutput)
1 0 0 0 0 0 0
(Conclusion)
1
[1]
I:
(ShapeONE)
0 1 0 0 0 0 0
T:
(ShapeONEOutput)
0 1 0 0 0 0 0
(Conclusion)
0
;

name: "Cause on Feature (ShapeTWO)"
freq: 0.5
2
# 2 events: RS event and GCy event
[0]
I:
(ShapeTWO)
1 0 0 0 0 0 0
T:
(ShapeTWOOutput)
1 0 0 0 0 0 0
(Conclusion)
1
[1]
I:
(ShapeTWO)
0 1 0 0 0 0 0
T:
(ShapeTWOOutput)
0 1 0 0 0 0 0
(Conclusion)
0
;