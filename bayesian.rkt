#lang racket

(require racket/match)
(require racket/vector)
(require test-engine/racket-tests)
(require racket/class)
(require racket/class)
(require racket/dict) ; For probabilities
(require racket/stream) ;?
(require net/url)
(require rackunit)

(require plot)


; Bayesian network is simply a graphical model for representing conditional
; independence(or joint probability) between set of random variable !



(provide (contract-out [num_states positive?]))
(define num_states 5)
(define Empty 0)

;; name of node, 
;;distr distribution of node
;; paarent, parent node
;; nodes, child nodes
;; current evidence
(define-struct node (name distr nodes parents childs evidence) #:prefab)
;;name - name of node,  distr - name of values, prob - values
(struct simple_node (name distr prob) #:prefab)

; Определение нода.
; (make-param "True" 0.08)
(define-struct param (name value))
(define-struct CheckGraph
  (left elem right))

(define-struct bayesian (parents last))

;; make_node -> node struct
(define (make_node name)
  (node name))

;(check-expect (send (new bayesian_network%)create-node "Radio" '("T" "F") '(0.2 0.8))"A")
;(check-expect (create-node "Difficulty" '("T" "F") '(0.6 0.4)) "A")


;;Создание связей THISSS
;; Node -> (Node -> Node)
(define (connect-node node1 node2)
  (let ([parent (append (node-childs node1) node2)])
    (cond [(null? node1)"Empty"]
          [else parent])))


(define belief%
  (class object%
    (init value)
    (define/public (connect_node name)
      name)))
; Class Construct basic bayesian network

; Доступ должен происходить -> Имя -> Параметр вероятности
;P(A|B) - conditional probability for unseen nodes
(define bayesian_network%
  (class object%
    (init defaultvalues)
    (define default-values defaultvalues)
    
    ;(dict-set! base "SUM" "FUN")
    ;(dict-ref base "SUM") -> "FUN"
    (define base (make-hash))
    (define graph (make-hash))
    (define freq (make-hash))
    (field (name "A"))
    (define (s) "A")
    ;(send (new bayesian_network%)create-node "Radio" '("T" "F") '(0.2 0.8))
    ;;(name of node, distrubutions, name of values) -> node
    ;; value of node -> Simple node
    (define/public (create-node name distr values parents childs)
      (cond [(< (length distr)(length values))name]      
      (else (make-node name distr values parents childs '()))))
    
    ; Расчитываем неизвестные величины
    ; value and value2 <= 1
    (define/private (simple_bayes_rule  value1 value2)
      ( / ( * value1  value2) (+ (* value1 value2) ( * (- 1 value1) (- 1 value2)))))
    
    ; Создание базового нода (без родителей)
    ; На вход получаем имя нода и список вероятностей
    ; Во второй части проходим по данному списку и заносим эти вероятности в хэш таблицу
    ; Чтобы можно было получить доступ по ключу
    ; A(TRUE)
    ; create-node-simple "Alarm" 
    (define/public (create-node-simple name values)
      (cond [ (= (length values) 0) "Error to construct node"]
            (else
              ; Проходим по списку с вероятностями
             ; В этот хэш помещаем хэши с вероятностямиы
             
             (hash-set! base name (make-hash))
             ;Матрица вероятностей
             (define weight (make-hash))
             (define hashparam (hash-ref base name))
             (define get-probabilities-list
               (λ (vals)
                    (cond [(empty? vals) (hash-set! hashparam "F" Empty)]
                          [else
                           (let* [(current (car vals))]
                               (hash-set! hashparam (param-name current) (param-value current)))])values))
             (get-probabilities-list values)
             )))
    
    
    (define/private (create-edge node nodes)
      (dict-set! graph node nodes))
    
    ; Создание нода из уже существующих
    ;name - string: name of node
    ;nodes -list of nodes
    ; probabilities - list of probs
    ;(name, nodes) -> nodes with parents
    (define/public (construct-node name vars nodes prob)
      (cond [(or 
              (empty? nodes)
              (empty? prob)
              (empty? vars))
              '()]
            [else
             
             (lambda (x)(
                         (create-edge name nodes)))]))
    
    
    (define/public (construct-node2 name vars values)
      (dict-set! graph name values))
                         
    
    ;; Ожидаемое количество состояний defaultvalues ** length parents
    (define/private (expected-count state)
      (expt default-values (length (node-parents state))))
    
    ;; Устанавливаем желаемый отклик Evidence
    ;; list of states ((list "Alarm" True) (list "Earthquake" False))
    ;; При каждой итерации делаем запрос к базе (создать базу) скорее всего хэширование
    (define (compute-evidence states)
      (cond [(empty? states)0]
            [else 
             (let()
               (car states))]))
    
    ; Connect from root node to all of nodes
    ; Intelegence -> Simple -> Hard
    (define/public (connect_node root nodes)
      (cond [(empty? nodes)0]
            [else (node-childs root)]))
    
    ;Conditional probability of x in evidence e
    ; Example Sum(P(X|E)
    
    
    ; Возвращаем вероятность определённого события на основе предоставленных данных
    ; Пример запроса (get_probability(A, '(("True" B) ("True" C))))
    (define/public (get_probability variables evidence)
      (cond [(empty? evidence)0]
      ))
    
    ; Получить информацию о вероятности в нодн
    
    ; (get-node-info "Burglary" "True")
    ;(define/public (get-node-info state param)
    ;  (node-distr state))
    
    ; Запрос везде где True
    
    ; (define/public (allTrue variables evidence)
    
    ; Предварительное количество нодом
    ;define
    
    ; Сэмплирование по Гиббсу or Gausian approximation
    ;http://www2.warwick.ac.uk/fac/sci/statistics/crism/research/2011/paper11-21/11-21w.pdf
    ;http://darrenjw.wordpress.com/2011/07/16/gibbs-sampler-in-various-languages-revisited/
    ; Probabilities N count of iteration
    (define/public (gibbs_sampling probabilities N)
      (let [(x 0)(y 0)]
        (lambda (x values)( (cond
                       [(equal? x 0)values]
                       [else 
                        (random-seed values
                                     (x x x))])))))
    
    
    ; check d-separation of components
    ; In values - can be few params
    ; D-separation is conditional independency of DAG
    ;All d-separations are Conditional Independencies
    (define/public (is_d_separated values)
      values)
    
    ; Blocked chain on graph
    ; Chain - Names of nodes
    ; Neopolitan 82
    (define/public (is_blocked chain)
      chain)
      
    
    ;;Update probability after some of iterations
    (define/public (update_probability prob)
      (+ 1 prob))
    (super-new)))


; Immutable bayesian network, can't update probabilities
(define immutabe_bayesian_network%
  (class object%
    (super-new)
    (define ivec (vector-immutable/c))
    (define lastAppend (vector-immutable/c))
    (define weight (vector-immutable/c))))


(define distributions%
  (class object%
    (field (consta 4))
    
    ; Дискретное равномерное распределение
    ; Уточнить
    (define/public (uniform-distribution-vector count bound)
        (map random (map (lambda (x) bound) (range count))))
    
    ; Геометрическое распределение
    ; http://www.iro.umontreal.ca/~simardr/ssj/doc/html/umontreal/iro/lecuyer/probdist/GeometricDist.html
    (define/public (geometric-dist p sample)
      (expt (- 1 p) sample))
    
    (define/public (normal-distribution x mu)
                   (x))
    ))
      

(define b (new bayesian_network% [defaultvalues (list "True" "False")]))
(define earthquake (send b create-node "Earthquake" '("True" "False") '(0.002 0.998) (list) (list)))
(define burgalary (send b create-node "Burglary" (list "True" "False") (list 0.012 0.999) (list) (list)))

;(define earthquake_value (send b get-node-info "Earthquake" "True"))
                         

;(define get-earthquake (send b get-node-info "Burglary" "True"))


;(define somequake (send b create-node "FUN" (list "True" "False")

;(define newEarth (send b (create-node "Earthquake" '("True" 0.002))))

; Если нод содержит несколько родителей, перемножаем их на заявленные величины
;(define alarm (send b create-node "Alarm" '(0.42 0.36) '(earthquake burgalary) (list)))

;New function for construct node
(define alarm2 (send b construct-node2 
                     "Alarm"
                     '("True" "False")
                     '( (send b set-value '("True" "Earthquake") 0.95)
                        (send b set-value '("False" "Earthquake") 0.05)
                        (send b set-value '("True" )))))

;Использовать только list!
(define earth (send b create-node-simple "Burglary" 
                    (list(make-param "True" 0.002)
                      (make-param "False" 0.998))))

(define alarm3 (send b create-node-simple "Alarm"

                     (list(make-param "True" 0.008)
                       (make-param "False" 0.992))))
; Test param struct
(define d (make-param "A" "B"))
(define e (make-param "C" "VALUE"))
(define l '(d e))



(define ali-calls (send b construct-node "Ali-calls" '("True" "False") '(alarm) '()))
;(define alarm (send b create-node "Alarm" (list "True" "false") (list ) (list earthquake burgalary) (list)))
; Динамическая Байесовская сеть
(define (dynamic_bayesian_network nodes)
  (let ([pons 4])
    pons))


;Функция максимального правдоподобия
(define (likelihood x y params)
  params)

;Факторизация (Factorization) произведение вероятностей в сети
; P(C,A,R,E,B) = P(B)P(E|B)P(R|E,B)P(A|R,B,E)P(C|A,R,B,E)

; Bayesian Network -> Factorization result
(define (Factorizarion btwork)
  (cond [(empty? btwork)0]))




; Test cases area of Bayesian network
; Check test case in racket docks
(test-case
 "Test create several type of nodes"
 (let ([ b (new bayesian_network% [defaultvalues (list "True" "False")])])
   (check-equal? (send b create-node "Earthquake" '("True" "false") '(0.002 0.998) (list) (list))
                 '#s(node "Earthquake" ("True" "false") (0.002 0.998) () () ()))
   
   (check-equal? 2 2)))


; Пример запроса к базе
; Alarm True (Получаем доступ к ноду по имени Alarm и берём значение True)
(test-case
 "Compute probability of nodes"
 )


(define (simple_test_bayes_rule  value1 value2)
      ( / ( * value1  value2) (+ (* value1 value2) ( * (- 1 value1) (- 1 value2)))))

