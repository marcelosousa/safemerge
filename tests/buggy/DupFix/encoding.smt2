(set-logic HORN)
(declare-fun Q_n0 (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_exit (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n0_1 (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1 (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1_1 (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n2 (Int Int Int Int Int Int Int Int) Bool)
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (and (= ro ra) (= ra rb) (= rb rm) (= xo xa) (= xa xb) (= xb xm)) (Q_n0 ro ra rb rm xo xa xb xm))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_n0 ro ra rb rm xo xa xb xm) (Q_n0_1 ro ra rb rm xo xa xb xm))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_n0_1 ro ra rb rm xo xa xb xm) (and (and (or (= ro ra) (= rm ra)) (or (= ro rb) (= rm rb)) (or (not (= ro ra)) (not (= ro rb)) (= rm ro))) (and (or (= xo xa) (= xm xa)) (or (= xo xb) (= xm xb)) (or (not (= xo xa)) (not (= xo xb)) (= xm xo)))))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int) (xa1 Int) (xm1 Int)) (=> (and (= xm1 (+ xm 1)) (= xa1 (+ xa 1)) (Q_n0_1 ro ra rb rm xo xa xb xm)) (Q_n1 ro ra rb rm xo xa1 xb xm1))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_n1 ro ra rb rm xo xa xb xm) (and (and (or (= ro ra) (= rm ra)) (or (= ro rb) (= rm rb)) (or (not (= ro ra)) (not (= ro rb)) (= rm ro))) (and (or (= xo xa) (= xm xa)) (or (= xo xb) (= xm xb)) (or (not (= xo xa)) (not (= xo xb)) (= xm xo)))))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_n1 ro ra rb rm xo xa xb xm) (Q_n1_1 ro ra rb rm xo xa xb xm))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_n1_1 ro ra rb rm xo xa xb xm) (and (and (or (= ro ra) (= rm ra)) (or (= ro rb) (= rm rb)) (or (not (= ro ra)) (not (= ro rb)) (= rm ro))) (and (or (= xo xa) (= xm xa)) (or (= xo xb) (= xm xb)) (or (not (= xo xa)) (not (= xo xb)) (= xm xo)))))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int) (xb1 Int) (xm1 Int)) (=> (and (= xm1 (+ xm 1)) (= xb1 (+ xb 1)) (Q_n1_1 ro ra rb rm xo xa xb xm)) (Q_n2 ro ra rb rm xo xa xb1 xm1))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_n2 ro ra rb rm xo xa xb xm) (and (and (or (= ro ra) (= rm ra)) (or (= ro rb) (= rm rb)) (or (not (= ro ra)) (not (= ro rb)) (= rm ro))) (and (or (= xo xa) (= xm xa)) (or (= xo xb) (= xm xb)) (or (not (= xo xa)) (not (= xo xb)) (= xm xo)))))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int) (ro1 Int) (ra1 Int) (rb1 Int) (rm1 Int)) (=> (and (= rm1 xm) (= rb1 xb) (= ra1 xa) (= ro1 xo) (Q_n2 ro ra rb rm xo xa xb xm)) (Q_exit ro1 ra1 rb1 rm1 xo xa xb xm))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_exit ro ra rb rm xo xa xb xm) (and (and (or (= ro ra) (= rm ra)) (or (= ro rb) (= rm rb)) (or (not (= ro ra)) (not (= ro rb)) (= rm ro))) (and (or (= xo xa) (= xm xa)) (or (= xo xb) (= xm xb)) (or (not (= xo xa)) (not (= xo xb)) (= xm xo)))))))
(assert (forall ((ro Int) (ra Int) (rb Int) (rm Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_exit ro ra rb rm xo xa xb xm) (and (and (or (= ro ra) (= rm ra)) (or (= ro rb) (= rm rb)) (or (not (= ro ra)) (not (= ro rb)) (= rm ro))) (and (or (= xo xa) (= xm xa)) (or (= xo xb) (= xm xb)) (or (not (= xo xa)) (not (= xo xb)) (= xm xo)))))))
(check-sat)
