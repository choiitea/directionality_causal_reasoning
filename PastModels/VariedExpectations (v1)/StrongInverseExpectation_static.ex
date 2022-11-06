#frequency 5:5 ratio
defT:-

#Red Square + Purple Diamond
freq: .2
name: "Red Square + Purple Diamond"
I:
(ShapeONE)
1 0 0 0 0 0 0
(ShapeTWO)
0 0 0 1 0 0 0
T:
(ShapeONEOutput)
1 0 0 0 0 0 0
(ShapeTWOOutput)
0 0 0 1 0 0 0
;

#Purple Diamond + Red Square
freq: .8
name: "Purple Diamond + Red Square"
I:
(ShapeONE)
0 0 0 1 0 0 0
(ShapeTWO)
1 0 0 0 0 0 0
T:
(ShapeONEOutput)
0 0 0 1 0 0 0
(ShapeTWOOutput)
1 0 0 0 0 0 0
;

#Green Cylinder + Yellow Circle
freq: .2
name: "Green Cylinder + Yellow Circle"
I:
(ShapeONE)
0 1 0 0 0 0 0
(ShapeTWO)
0 0 0 0 1 0 0
T:
(ShapeONEOutput)
0 1 0 0 0 0 0
(ShapeTWOOutput)
0 0 0 0 1 0 0
;

#Yellow Circle + Green Cylinder
freq: .8
name: "Yellow Circle + Green Cylinder"
I:
(ShapeONE)
0 0 0 0 1 0 0
(ShapeTWO)
0 1 0 0 0 0 0
T:
(ShapeONEOutput)
0 0 0 0 1 0 0
(ShapeTWOOutput)
0 1 0 0 0 0 0
;
