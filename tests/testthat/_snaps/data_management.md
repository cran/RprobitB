# overview effects can be generated

    Code
      out
    Output
            effect as_value as_coef random
      1 income_bus    FALSE    TRUE  FALSE
      2 income_car    FALSE    TRUE  FALSE
      3   time_bus     TRUE    TRUE  FALSE
      4   time_car     TRUE    TRUE  FALSE
      5 time_train     TRUE    TRUE  FALSE
      6       cost     TRUE   FALSE   TRUE
      7    ASC_bus    FALSE    TRUE   TRUE
      8    ASC_car    FALSE    TRUE   TRUE

# data preparation works

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
                       count
      deciders           235
      choice occasions  5-19
      total choices     2929
      alternatives         2
      - 'A'             1474
      - 'B'             1455

# data preparation with ordered choices works

    Code
      print(data)
    Output
      Empirical data of 55 (ordered) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         5
      - 'very bad'        34
      - 'bad'             17
      - 'indifferent'      4
      - 'good'             0
      - 'very good'        0

# data preparation with ranked choices works

    Code
      print(data)
    Output
      Empirical data of 55 (ranked) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         6
      - 'A,B,C'            3
      - 'A,C,B'            9
      - 'B,A,C'            4
      - 'B,C,A'           17
      - 'C,A,B'            7
      - 'C,B,A'           15

# data preparation with non-standard base alternative works

    Code
      print(data)
    Output
      Empirical data of 2929 choices.

---

    Code
      summary(data)
    Output
                       count
      deciders           235
      choice occasions  5-19
      total choices     2929
      alternatives         2
      - 'A'             1474
      - 'B'             1455

# simulating choice data works

    Code
      print(data)
    Output
      Simulated data of 55 choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         3
      - 'bus'              7
      - 'car'             22
      - 'train'           26

# simulating ordered choices works

    Code
      print(data)
    Output
      Simulated data of 55 (ordered) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         5
      - 'very bad'        34
      - 'bad'             17
      - 'indifferent'      4
      - 'good'             0
      - 'very good'        0

# simulating ranked choices works

    Code
      print(data)
    Output
      Simulated data of 55 (ranked) choices.

---

    Code
      summary(data)
    Output
                       count
      deciders            10
      choice occasions  1-10
      total choices       55
      alternatives         6
      - 'A,B,C'            3
      - 'A,C,B'            9
      - 'B,A,C'            4
      - 'B,C,A'           17
      - 'C,A,B'            7
      - 'C,B,A'           15

# splitting data set by N works

    Code
      train_test(x, test_proportion = 0.3, by = "N")
    Output
      $train
      Simulated data of 28 choices.
      
      $test
      Simulated data of 27 choices.
      

---

    Code
      train_test(x, test_proportion = 0, by = "N")
    Output
      $train
      Simulated data of 55 choices.
      
      $test
      Simulated data of 0 choices.
      

---

    Code
      train_test(x, test_proportion = 1, by = "N")
    Output
      $train
      Simulated data of 0 choices.
      
      $test
      Simulated data of 55 choices.
      

---

    Code
      train_test(x, test_proportion = 0.5, by = "N", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 32 choices.
      
      $test
      Simulated data of 23 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "N")
    Output
      $train
      Simulated data of 45 choices.
      
      $test
      Simulated data of 10 choices.
      

---

    Code
      train_test(x, test_number = 2, by = "N")
    Output
      $train
      Simulated data of 36 choices.
      
      $test
      Simulated data of 19 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "N", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 46 choices.
      
      $test
      Simulated data of 9 choices.
      

# splitting data set by T works

    Code
      train_test(x, test_proportion = 0.3, by = "T")
    Output
      $train
      Simulated data of 70 choices.
      
      $test
      Simulated data of 30 choices.
      

---

    Code
      train_test(x, test_proportion = 0.5, by = "T", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 50 choices.
      
      $test
      Simulated data of 50 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "T")
    Output
      $train
      Simulated data of 90 choices.
      
      $test
      Simulated data of 10 choices.
      

---

    Code
      train_test(x, test_number = 2, by = "T")
    Output
      $train
      Simulated data of 80 choices.
      
      $test
      Simulated data of 20 choices.
      

---

    Code
      train_test(x, test_number = 1, by = "T", random = TRUE, seed = 1)
    Output
      $train
      Simulated data of 90 choices.
      
      $test
      Simulated data of 10 choices.
      

# parameter reproducibility works

    Code
      unclass(x)
    Output
      $alpha
         1    2 
      -1.4 -0.8 
      
      $C
      [1] 1
      
      $s
      [1] 1
      
      $b
           [,1]
      [1,]  0.4
      [2,]  2.4
      attr(,"names")
      [1] "1.1" "1.2"
      
      $Omega
                 [,1]
      [1,]  0.1673803
      [2,] -0.3798966
      [3,] -0.3798966
      [4,]  0.8729389
      attr(,"names")
      [1] "1.1,1" "1.1,2" "1.2,1" "1.2,2"
      
      $Sigma
               [,1]
      [1,] 1.849173
      attr(,"names")
      [1] "1,1"
      
      $Sigma_full
                [,1]      [,2]
      [1,] 2.0407798 0.1506006
      [2,] 0.1506006 0.1095942
      attr(,"names")
      [1] "1,1" "1,2" "2,1" "2,2"
      
      $beta
               [,1]       [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
      [1,] 0.329376 -0.1169716 0.3954811 0.3526134 0.4991152 0.5497143 0.4267108
      [2,] 2.330227  3.6104617 2.3129450 2.4232419 2.0276136 2.0858978 2.3413573
                [,8]      [,9]   [,10]     [,11]      [,12]     [,13]     [,14]
      [1,] 0.5052826 0.3512455 0.85043 0.3518245 -0.1881472 0.9130722 0.3101915
      [2,] 2.0939030 2.5793620 1.39255 2.4149873  3.6524351 1.3153790 2.5598877
               [,15]     [,16]     [,17]      [,18]      [,19]    [,20]     [,21]
      [1,] 0.2285863 0.2871733 0.6645683 0.04272983 0.03961658 0.448979 0.9956759
      [2,] 2.8921908 2.7860154 1.9339352 3.21174840 3.27963230 2.259643 1.0717096
               [,22]      [,23]    [,24]    [,25]     [,26]     [,27]       [,28]
      [1,] 0.8077074 0.08220411 0.419057 0.635948 1.0650052 1.0866272 0.002216705
      [2,] 1.5555266 3.05756412 2.239807 1.731983 0.8388654 0.7989128 3.305459398
               [,29]    [,30]     [,31]     [,32]     [,33]      [,34]    [,35]
      [1,] 0.4112407 0.831112 0.5373082 0.4564803 0.4808769 0.07138833 1.046449
      [2,] 2.2006700 1.305698 2.1395443 2.2595196 2.1058791 3.03061670 1.087730
               [,36]     [,37]     [,38]       [,39]       [,40]      [,41]     [,42]
      [1,] 0.5074539 0.3984766 0.2053817 0.001514765 0.008945824 0.00851511 0.0274211
      [2,] 2.0285707 2.5598422 2.9242634 3.375743273 3.160138804 3.19855796 3.3223142
               [,43]       [,44]      [,45]     [,46]       [,47]      [,48]
      [1,] 0.4280295 -0.04451158 0.08588065 0.2166424 -0.08296393 -0.1365302
      [2,] 2.3028900  3.30379159 2.99710799 2.8649616  3.64826495  3.6077563
               [,49]     [,50]     [,51]     [,52]     [,53]     [,54]     [,55]
      [1,] 1.3695030 0.2968265 0.6383484 0.2466389 0.2000192 1.0874377 0.6918019
      [2,] 0.2916928 2.5446045 1.8577337 2.7809635 3.1289308 0.9203981 1.6815464
               [,56]       [,57]      [,58]     [,59]     [,60]     [,61]      [,62]
      [1,] 0.7623908 -0.01241664 0.01993811 0.1484107 0.6450535 0.3156437 -0.1687466
      [2,] 1.5414328  3.53086421 3.23217687 2.8730462 1.6861913 2.5320486  3.6835776
              [,63]     [,64]     [,65]      [,66]    [,67]     [,68]     [,69]
      [1,] 0.223718 0.8013956 0.3629925 0.09835005 0.850924 0.4661912 0.1121037
      [2,] 2.738834 1.5440464 2.5001837 3.06381429 1.374823 2.4592329 3.1528226
               [,70]     [,71]     [,72]      [,73]    [,74]     [,75]     [,76]
      [1,] 1.1325256 0.4072155 0.1991898 0.03334085 0.642953 1.0371535 0.7340707
      [2,] 0.6273252 2.3432867 2.7475901 3.36351217 1.928819 0.9160759 1.6354998
               [,77]    [,78]     [,79]     [,80]     [,81]      [,82]     [,83]
      [1,] 0.1948754 0.415112 0.3024424 0.8795551 0.4388895 -0.2645622 0.1783543
      [2,] 2.9613659 2.255401 2.7761057 1.1607703 2.3994261  4.0540493 2.9318891
               [,84]     [,85]      [,86]     [,87]     [,88]     [,89]     [,90]
      [1,] 0.3206416 -0.203678 0.01002425 1.3120387 0.3747542 1.0306986 0.6031607
      [2,] 2.7431725  3.755185 3.32717045 0.1733037 2.4420641 0.8669531 2.1144456
               [,91]     [,92]       [,93]     [,94]       [,95]        [,96]
      [1,] 0.2933272 0.3340584 -0.01466071 0.4023083 -0.05296476 0.0003578565
      [2,] 2.5690818 2.6015275  3.50818798 2.0942450  3.58817479 3.2965517833
               [,97]     [,98]     [,99]    [,100]
      [1,] 0.4174491 0.6008652 1.1666541 0.4334703
      [2,] 2.1952136 1.9877194 0.7669766 2.3154964
      
      $z
        [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      
      $d
      [1] NA
      

# parameter printing works

    Code
      x
    Output
      alpha : -1.4
      
      C : 1
      
      s : 1
      
      b : -0.8
      
      Omega : 1.109553
      
      Sigma : 3.879587
      
      Sigma_full : 2 x 2 matrix of doubles 
      
                 [,1]       [,2]
      [1,]  1.9936042 -0.5292764
      [2,] -0.5292764  0.8274302
      
      
      beta : 1 x 100 matrix of doubles 
      
              [,1]    [,2]    [,3] ...  [,100]
      [1,] -0.9818 -3.1426 -2.1310 ... -1.7120
      
      
      z : integer vector of length 100 
      
      1 1 1 ... 1
      
      d : NA
      
