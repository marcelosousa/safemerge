(set-logic HORN)
(declare-fun Q_n0 (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_exit (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n0_1b (Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1 (Int Int Int Int Int Int Int Int) Bool)
(assert (forall ((ko Int) (ka Int) (kb Int) (km Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (and (= ko ka) (= ka kb) (= kb km) (= xo xa) (= xa xb) (= xb xm)) (Q_n0 ko ka kb km xo xa xb xm))))
(assert (forall ((ko Int) (ka Int) (kb Int) (km Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_n0 ko ka kb km xo xa xb xm) (Q_n0_1b ko ka kb km xo xa xb xm))))
(assert (forall ((ko Int) (ka Int) (kb Int) (km Int) (xo Int) (xa Int) (xb Int) (xm Int) (kb1 Int) (km1 Int)) (=> (and (= km1 3) (= kb1 3) (Q_n0_1b ko ka kb km xo xa xb xm)) (Q_n1 ko ka kb1 km1 xo xa xb xm))))
(assert (forall ((ko Int) (ka Int) (kb Int) (km Int) (xo Int) (xa Int) (xb Int) (xm Int) (xo1 Int) (xa1 Int) (xb1 Int) (xm1 Int)) (=> (and (= xm1 1) (= xb1 1) (= xa1 1) (= xo1 1) (Q_n1 ko ka kb km xo xa xb xm)) (Q_exit ko ka kb km xo1 xa1 xb1 xm1))))
(assert (forall ((ko Int) (ka Int) (kb Int) (km Int) (xo Int) (xa Int) (xb Int) (xm Int)) (=> (Q_exit ko ka kb km xo xa xb xm) (and (and (or (= ko ka) (= km ka)) (or (= ko kb) (= km kb)) (or (not (= ko ka)) (not (= ko kb)) (= km ko))) (and (or (= xo xa) (= xm xa)) (or (= xo xb) (= xm xb)) (or (not (= xo xa)) (not (= xo xb)) (= xm xo)))))))
(check-sat)
