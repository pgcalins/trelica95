 GiD Post Results File 1.0
 GaussPoints "Malha_gauss" ElemType Linear "Malha"
 Number Of Gauss Points: 1
 Nodes not included
 Natural Coordinates: Internal
 End gausspoints
 Result "Desloc" "Load Analysis"            1  Vector OnNodes
 ComponentNames "X-Desloc", "Y-Desloc"
 Values
         1      0.47422E-02     -0.10100E-01
         2      0.20218E-02     -0.11267E-01
         3      0.00000E+00      0.00000E+00
         4      0.18941E-02     -0.12235E-01
         5      0.35944E-02     -0.15922E-01
         6      0.61262E-02      0.00000E+00
 End Values
 Result "Reac" "Load Analysis"            1  Vector OnNodes
 ComponentNames "X-Reac", "Y-Reac"
 Values
         1     -0.14065E-10      0.17703E-10
         2      0.25580E-11     -0.89670E-11
         3      0.72760E-10      0.66811E+05
         4     -0.14552E-10     -0.44500E+05
         5     -0.14552E-10     -0.11125E+06
         6     -0.29104E-10      0.88939E+05
 End Values
 Result "Forc" "Load Analysis"            1  Scalar OnGaussPoints "Malha_gauss"
 Values
         1    -0.9596E+05
         2    -0.9448E+05
         3     0.3766E+05
         4     0.4122E+05
         5     0.9668E+04
         6     0.8210E+05
         7    -0.1259E+06
         8     0.6681E+05
         9     0.5997E+05
        10     0.8912E+05
 End Values
