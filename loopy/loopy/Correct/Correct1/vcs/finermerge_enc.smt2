(set-logic HORN)
(declare-fun Q_n0 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n6 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n2 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n3 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n3_a ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n4 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n5 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n5_a ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int) Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (and (forall ((i Int)) (= (select ao i) (select aa i))) (forall ((i Int)) (= (select aa i) (select ab i))) (forall ((i Int)) (= (select ab i) (select am i))) (forall ((i Int)) (= (select bo i) (select ba i))) (forall ((i Int)) (= (select ba i) (select bb i))) (forall ((i Int)) (= (select bb i) (select bm i))) (= io ia) (= ia ib) (= ib im) (= no na) (= na nb) (= nb nm) (= reto reta) (= reta retb) (= retb retm)) (Q_n0 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (io1 Int) (ia1 Int) (ib1 Int) (im1 Int)) (=> (Q_n0 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) true)))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (io1 Int) (ia1 Int) (ib1 Int) (im1 Int)) (=> (and (= im1 0) (= ib1 0) (= ia1 0) (= io1 0) (Q_n0 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm)) (Q_n1 ao aa ab am bo ba bb bm io1 ia1 ib1 im1 no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n1 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) true)))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n1 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) (Q_n2 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n1 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) (Q_n5 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n2 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) (= (< io no) (< ia na) (< ib nb) (< im nm)))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (and (< im nm) (< ib nb) (< ia na) (< io no) (Q_n2 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm)) (Q_n3 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n3 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) true)))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n3 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) (Q_n3_a ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (ao1 (Array Int Int)) (aa1 (Array Int Int)) (ab1 (Array Int Int)) (am1 (Array Int Int))) (=> (Q_n3_a ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) true)))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (ao1 (Array Int Int)) (aa1 (Array Int Int)) (ab1 (Array Int Int)) (am1 (Array Int Int))) (=> (and (= (store am im (select bm im)) am1) (= (store ab ib (select bb ib)) ab1) (= (store aa ia 0) aa1) (= (store ao io 0) ao1) (Q_n3_a ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm)) (Q_n4 ao1 aa1 ab1 am1 bo ba bb bm io ia ib im no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (io1 Int) (ia1 Int) (ib1 Int) (im1 Int)) (=> (Q_n4 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) true)))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (io1 Int) (ia1 Int) (ib1 Int) (im1 Int)) (=> (and (= im1 (+ im 1)) (= ib1 (+ ib 1)) (= ia1 (+ ia 1)) (= io1 (+ io 1)) (Q_n4 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm)) (Q_n1 ao aa ab am bo ba bb bm io1 ia1 ib1 im1 no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n5 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) true)))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n5 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) (Q_n5_a ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (reta1 Int) (retm1 Int)) (=> (Q_n5_a ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) true)))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int) (reta1 Int) (retm1 Int)) (=> (and (= retm1 nm) (= reta1 na) (Q_n5_a ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm)) (Q_n6 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta1 retb retm1))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n6 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) (and (and (or (forall ((i Int)) (= (select ao i) (select aa i))) (forall ((i Int)) (= (select am i) (select aa i)))) (or (forall ((i Int)) (= (select ao i) (select ab i))) (forall ((i Int)) (= (select am i) (select ab i)))) (or (not (forall ((i Int)) (= (select ao i) (select aa i)))) (not (forall ((i Int)) (= (select ao i) (select ab i)))) (forall ((i Int)) (= (select am i) (select ao i))))) (and (or (forall ((i Int)) (= (select bo i) (select ba i))) (forall ((i Int)) (= (select bm i) (select ba i)))) (or (forall ((i Int)) (= (select bo i) (select bb i))) (forall ((i Int)) (= (select bm i) (select bb i)))) (or (not (forall ((i Int)) (= (select bo i) (select ba i)))) (not (forall ((i Int)) (= (select bo i) (select bb i)))) (forall ((i Int)) (= (select bm i) (select bo i))))) (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= no na) (= nm na)) (or (= no nb) (= nm nb)) (or (not (= no na)) (not (= no nb)) (= nm no))) (and (or (= reto reta) (= retm reta)) (or (= reto retb) (= retm retb)) (or (not (= reto reta)) (not (= reto retb)) (= retm reto)))))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (bo (Array Int Int)) (ba (Array Int Int)) (bb (Array Int Int)) (bm (Array Int Int)) (io Int) (ia Int) (ib Int) (im Int) (no Int) (na Int) (nb Int) (nm Int) (reto Int) (reta Int) (retb Int) (retm Int)) (=> (Q_n4 ao aa ab am bo ba bb bm io ia ib im no na nb nm reto reta retb retm) (and (and (or (forall ((i Int)) (= (select ao i) (select aa i))) (forall ((i Int)) (= (select am i) (select aa i)))) (or (forall ((i Int)) (= (select ao i) (select ab i))) (forall ((i Int)) (= (select am i) (select ab i)))) (or (not (forall ((i Int)) (= (select ao i) (select aa i)))) (not (forall ((i Int)) (= (select ao i) (select ab i)))) (forall ((i Int)) (= (select am i) (select ao i))))) (and (or (forall ((i Int)) (= (select bo i) (select ba i))) (forall ((i Int)) (= (select bm i) (select ba i)))) (or (forall ((i Int)) (= (select bo i) (select bb i))) (forall ((i Int)) (= (select bm i) (select bb i)))) (or (not (forall ((i Int)) (= (select bo i) (select ba i)))) (not (forall ((i Int)) (= (select bo i) (select bb i)))) (forall ((i Int)) (= (select bm i) (select bo i))))) (and (or (= io ia) (= im ia)) (or (= io ib) (= im ib)) (or (not (= io ia)) (not (= io ib)) (= im io))) (and (or (= no na) (= nm na)) (or (= no nb) (= nm nb)) (or (not (= no na)) (not (= no nb)) (= nm no))) (and (or (= reto reta) (= retm reta)) (or (= reto retb) (= retm retb)) (or (not (= reto reta)) (not (= reto retb)) (= retm reto)))))))
(check-sat)
