(declare-fun x () Int)
(declare-fun z () Bool)
(declare-fun w () Bool)
(assert (= z (= x 1)))
(assert (= w (= x 2)))
(check-sat)
(get-model)
(exit)

