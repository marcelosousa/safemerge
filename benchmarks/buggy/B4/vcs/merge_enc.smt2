(set-logic HORN)
(declare-fun Q_n0 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_exit (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n0_b (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1_1 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n1_a (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n2 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n2_1 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n2_a (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n3 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n3_1 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n3_a (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n3_b (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n4 (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(declare-fun Q_n4_a (Int Int Int Int Int Int Int Int Int Int Int Int) Bool)
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (and (= go ga) (= ga gb) (= gb gm) (= gradeo gradea) (= gradea gradeb) (= gradeb gradem) (= ro ra) (= ra rb) (= rb rm)) (Q_n0 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (Q_n0 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm) (Q_n0_b go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int) (gb1 Int) (gm1 Int)) (=> (and (= gm1 4) (= gb1 4) (Q_n0_b go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n1 go ga gb1 gm1 gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int) (gb1 Int) (gm1 Int)) (=> (and (= gm1 4) (= gb1 4) (Q_n0_b go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n2 go ga gb1 gm1 gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int) (gb1 Int) (gm1 Int)) (=> (and (= gm1 4) (= gb1 4) (Q_n0_b go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n3 go ga gb1 gm1 gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (Q_n1 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm) (Q_n1_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int) (ro1 Int) (ra1 Int) (rb1 Int) (rm1 Int)) (=> (and (= rm1 2) (= rb1 2) (= ra1 2) (= ro1 2) (Q_n1_1 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_exit go ga gb gm gradeo gradea gradeb gradem ro1 ra1 rb1 rm1))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (and (= gm 2) (= gradeb 2) (= ga 2) (= gradeo 2) (Q_n1_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n1_1 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (Q_n2 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm) (Q_n2_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int) (ro1 Int) (ra1 Int) (rb1 Int) (rm1 Int)) (=> (and (= rm1 1) (= rb1 1) (= ra1 1) (= ro1 1) (Q_n2_1 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_exit go ga gb gm gradeo gradea gradeb gradem ro1 ra1 rb1 rm1))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (and (= gm 1) (= gradeb 1) (= ga 1) (= gradeo 1) (Q_n2_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n2_1 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (Q_n3 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm) (Q_n3_b go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int) (ro1 Int) (ra1 Int) (rb1 Int) (rm1 Int)) (=> (and (= rm1 0) (= rb1 0) (= ra1 0) (= ro1 0) (Q_n3_1 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_exit go ga gb gm gradeo gradea gradeb gradem ro1 ra1 rb1 rm1))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (and (= gm 0) (= gradeb 0) (= ga 0) (Q_n3_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n3_1 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (and (= gradeo 0) (Q_n3_b go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n3_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (and (= gradeo 0) (Q_n3_b go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n4 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (and (= gradem 4) (= gradeb 4) (Q_n4 go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_n4_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int) (rb1 Int) (rm1 Int)) (=> (and (= rm1 gm) (= rb1 gb) (Q_n4_a go ga gb gm gradeo gradea gradeb gradem ro ra rb rm)) (Q_exit go ga gb gm gradeo gradea gradeb gradem ro ra rb1 rm1))))
(assert (forall ((go Int) (ga Int) (gb Int) (gm Int) (gradeo Int) (gradea Int) (gradeb Int) (gradem Int) (ro Int) (ra Int) (rb Int) (rm Int)) (=> (Q_exit go ga gb gm gradeo gradea gradeb gradem ro ra rb rm) (and (and (or (= go ga) (= gm ga)) (or (= go gb) (= gm gb)) (or (not (= go ga)) (not (= go gb)) (= gm go))) (and (or (= gradeo gradea) (= gradem gradea)) (or (= gradeo gradeb) (= gradem gradeb)) (or (not (= gradeo gradea)) (not (= gradeo gradeb)) (= gradem gradeo))) (and (or (= ro ra) (= rm ra)) (or (= ro rb) (= rm rb)) (or (not (= ro ra)) (not (= ro rb)) (= rm ro)))))))
(check-sat)
(get-model)
