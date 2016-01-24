(set-logic HORN)
(declare-fun Q_n0 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int)) Bool)
(declare-fun Q_n1 ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int)) Bool)
(declare-fun Q_n0_a ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int)) Bool)
(declare-fun Q_n0_b ((Array Int Int) (Array Int Int) (Array Int Int) (Array Int Int)) Bool)
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int))) (=> (and (forall ((i Int)) (= (select ao i) (select aa i))) (forall ((i Int)) (= (select aa i) (select ab i))) (forall ((i Int)) (= (select ab i) (select am i)))) (Q_n0 ao aa ab am))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int))) (=> (Q_n0 ao aa ab am) (Q_n0_a ao aa ab am))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (ao1 (Array Int Int)) (aa1 (Array Int Int)) (am1 (Array Int Int))) (=> (and (= (store am 0 1) am1) (= (store aa 0 1) aa1) (= (store ao 1 1) ao1) (Q_n0_a ao aa ab am)) (Q_n0_b ao1 aa1 ab am1))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int)) (ab1 (Array Int Int)) (am1 (Array Int Int))) (=> (and (= (store am 0 1) am1) (= (store ab 0 1) ab1) (Q_n0_b ao aa ab am)) (Q_n1 ao aa ab1 am1))))
(assert (forall ((ao (Array Int Int)) (aa (Array Int Int)) (ab (Array Int Int)) (am (Array Int Int))) (=> (Q_n1 ao aa ab am) (and (and (or (forall ((i Int)) (= (select ao i) (select aa i))) (forall ((i Int)) (= (select am i) (select aa i)))) (or (forall ((i Int)) (= (select ao i) (select ab i))) (forall ((i Int)) (= (select am i) (select ab i)))) (or (not (forall ((i Int)) (= (select ao i) (select aa i)))) (not (forall ((i Int)) (= (select ao i) (select ab i)))) (forall ((i Int)) (= (select am i) (select ao i)))))))))
(check-sat)
