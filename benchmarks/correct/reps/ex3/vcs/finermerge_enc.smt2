(set-logic HORN)
(declare-fun myplus (Int Int) Int)
(declare-fun myleq (Int Int) Int)
(declare-fun mylt (Int Int) Int)
(declare-fun Q_start (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_exit (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n0 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n10 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n11 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n11_a (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n12 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n13 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n14 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1_a (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n2 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n3 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n4 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n5 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n5_a (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n6 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n61 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n6_b (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n7 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n8 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n8_b (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n9 (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (and (= io ia) (= ia ib) (= ib im) (= jo ja) (= ja jb) (= jb jm) (= ko ka) (= ka kb) (= kb km) (= lenio lenia) (= lenia lenib) (= lenib lenim) (= printvaro printvara) (= printvara printvarb) (= printvarb printvarm) (= twoio twoia) (= twoia twoib) (= twoib twoim)) (Q_start io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (io1 Int) (ia1 Int) (ib1 Int) (im1 Int)) (=> (and (= im1 1) (= ib1 1) (= ia1 1) (= io1 1) (Q_n0 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n1 io1 ia1 ib1 im1 jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n1 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n1_a io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (jo1 Int) (ja1 Int) (jb1 Int) (jm1 Int)) (=> (and (= jm1 (myplus jm 1)) (= jb1 (myplus jb 1)) (= ja1 (myplus ja 1)) (= jo1 (myplus jo 1)) (Q_n10 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n61 io ia ib im jo1 ja1 jb1 jm1 ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n11 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n11_a io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (twoia1 Int) (twoim1 Int)) (=> (and (= twoim1 (+ twoim 2)) (= twoia1 (+ twoia 2)) (Q_n11_a io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n12 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia1 twoib twoim1))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (io1 Int) (ia1 Int) (ib1 Int) (im1 Int)) (=> (and (= im1 (+ im 1)) (= ib1 (+ ib 1)) (= ia1 (+ ia 1)) (= io1 (+ io 1)) (Q_n12 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n2 io1 ia1 ib1 im1 jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (printvaro1 Int) (printvara1 Int) (printvarb1 Int) (printvarm1 Int)) (=> (and (= printvarm1 km) (= printvarb1 kb) (= printvara1 ka) (= printvaro1 ko) (Q_n13 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n14 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro1 printvara1 printvarb1 printvarm1 twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n14 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_exit io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (twoia1 Int) (twoim1 Int)) (=> (and (= twoim1 2) (= twoia1 2) (Q_n1_a io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n2 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia1 twoib twoim1))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n2 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n3 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n2 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n4 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (and (not (= (myleq im 100) 0)) (not (= (myleq ib 100) 0)) (not (= (myleq ia 100) 0)) (not (= (myleq io 100) 0)) (Q_n3 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n5 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (and (= (myleq im 100) 0) (= (myleq ib 100) 0) (= (myleq ia 100) 0) (= (myleq io 100) 0) (Q_n4 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n13 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n5 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n5_a io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (jo1 Int) (ja1 Int) (jb1 Int) (jm1 Int)) (=> (and (= jm1 twoim) (= jb1 (* ib 2)) (= ja1 twoia) (= jo1 (* io 2)) (Q_n5_a io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n6 io ia ib im jo1 ja1 jb1 jm1 ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n6 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n6_b io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n61 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n7 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n61 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n9 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (lenib1 Int) (lenim1 Int)) (=> (and (= lenim1 (* im 10)) (= lenib1 (* ib 10)) (Q_n6_b io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n61 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib1 lenim1 printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (and (not (= (mylt jm 1000) 0)) (not (= (mylt jb 1000) 0)) (not (= (mylt ja 1000) 0)) (not (= (mylt jo 1000) 0)) (Q_n7 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n8 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n8 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (Q_n8_b io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (ko1 Int) (ka1 Int) (kb1 Int) (km1 Int)) (=> (and (= km1 (myplus km (myplus lenim jm))) (= kb1 (myplus kb (myplus lenib jb))) (= ka1 (myplus ka (myplus (* ia 10) ja))) (= ko1 (myplus ko (myplus (* io 10) jo))) (Q_n8_b io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n10 io ia ib im jo ja jb jm ko1 ka1 kb1 km1 lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (and (= (mylt jm 1000) 0) (= (mylt jb 1000) 0) (= (mylt ja 1000) 0) (= (mylt jo 1000) 0) (Q_n9 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n11 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int) (ko1 Int) (ka1 Int) (kb1 Int) (km1 Int)) (=> (and (= km1 0) (= kb1 0) (= ka1 0) (= ko1 0) (Q_start io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim)) (Q_n0 io ia ib im jo ja jb jm ko1 ka1 kb1 km1 lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_exit io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n1 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n2 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n61 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n12 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n14 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n3 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n4 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n5 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n13 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n6 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n7 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n9 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n8 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n10 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n11 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(assert (forall ((io Int) (ia Int) (ib Int) (im Int) (jo Int) (ja Int) (jb Int) (jm Int) (ko Int) (ka Int) (kb Int) (km Int) (lenio Int) (lenia Int) (lenib Int) (lenim Int) (printvaro Int) (printvara Int) (printvarb Int) (printvarm Int) (twoio Int) (twoia Int) (twoib Int) (twoim Int)) (=> (Q_n0 io ia ib im jo ja jb jm ko ka kb km lenio lenia lenib lenim printvaro printvara printvarb printvarm twoio twoia twoib twoim) (and (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= jo ja) (= jm ja)) (or (= jo jb) (= jm jb)) (or (not (= jo ja)) (not (= jo jb)) (= jm jo))) (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= lenio lenia) (= lenim lenia)) (or (= lenio lenib) (= lenim lenib)) (or (not (= lenio lenia)) (not (= lenio lenib)) (= lenim lenio))) (and (or (= printvaro printvara) (= printvarm printvara)) (or (= printvaro printvarb) (= printvarm printvarb)) (or (not (= printvaro printvara)) (not (= printvaro printvarb)) (= printvarm printvaro))) (and (or (= twoio twoia) (= twoim twoia)) (or (= twoio twoib) (= twoim twoib)) (or (not (= twoio twoia)) (not (= twoio twoib)) (= twoim twoio)))))))
(check-sat)
