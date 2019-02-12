#lang racket
(require racket/include)

(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
;Functiile de baza .. Nu cred ca este nevoie sa fie explicate 
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (list table (list columns-name) '())
))

(define get-name
  (λ (table)
     (car table)))

(define get-columns
  (λ (table)
    (car (second table) )))

(define get-tables
  (λ (db)
    (if (null? db)
        '()
      db)
))
(define get-table
  (λ (db table-name)
    (if (null? db)
        '()
        (if (equal? (caar db) table-name)
            (car db)
            (get-table (cdr db) table-name)
             )
        ))
)


(define add-table
  (λ (db table)
    (append db (list table))
   )
)
 
(define remove-table
  (λ (db table-name)
    (if (null? db)
        '()
        (remove (get-table db table-name) db)
    )
   )
)
;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
;tabela de cursuri 
(define Cursuri (list "Cursuri" 
                (list '( "Anul" "Semestru" "Disciplină" "Număr credite"  "Număr teme"))
                (list '("I" "II" "III" "IV" "I" "III")
                      '("I" "II" "I"   "I"  "II" "II")
                      '("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date")
                      '(5 6 5 6 5 5)
                      '(2 3 3 3 3 0)
                 )
           )
)
;tabela Studenti
(define Studenti (list "Studenți" 
                 (list '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie"))
                 (list '(123 124 125 126)
                      '("Ionescu" "Popescu" "Popa" "Georgescu")
                      '("Gigel" "Maria" "Ionel" "Ioana")
                      '("321CA" "321CB" "321CC" "321CD")
                      '(9.82 9.91 9.99 9.87)
                 )
           )
)
(define db (add-table (add-table (init-database) Studenti) Cursuri))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

;functia se bazeaza pe 2 cazuri : 1 cand tabela nu are nimic ca intrari si apeleaza empty-entries
;                                 2 cand tabela are ceva ca intrari si apeleaza make-table 
(define insert
  (λ (db table-name record)
        (if (equal? (get-table db table-name) '())
            '()
            (if (equal? (third (get-table db table-name)) '())
                (cons (empty-entries db table-name record) (remove-table db table-name))
                (cons (make-table db table-name record) (remove-table db table-name))
            )
       )
  )
)

;face o noua tabela unde are inserate noile campuri -> apeleaza change-records care se ocupa de modificarea intrarilor 
(define make-table
   (λ (db table-name records)
     (let ( (antet (list table-name (list (get-columns (get-table db table-name)))))
            (headers (get-columns (get-table db table-name)))
            (table (get-table db table-name))
          )
       (append antet (list (change-records db table headers records))) 
     )
   )
)

;functia care trateaza cazul cand tabela nu are intrarii 
(define empty-entries
   (λ (db table-name records)
     (let ( (antet (list table-name (list (get-columns (get-table db table-name)))))
            (headers (get-columns (get-table db table-name)))
            (table (get-table db table-name))
          )
       (append antet (list (insert-in-empty-entries db table headers records))) 
     )
   )
)

;o functie ajutatoare pentru functia de mai sus 
(define insert-in-empty-entries
  (λ (db table headers records)
    (if (null? headers)
        '()
        (append  (list (list (get-value (car headers) records)))
                    (insert-in-empty-entries db table (cdr headers) records))
    )
  )
)

;functie apelata din make-table pentru modificarea intrarilor 
(define change-records
  (λ (db table headers records)
    (if (null? headers)
        '()
        (append (list (append (get-records-column db (get-name table) (car headers)) (list (get-value (car headers) records))))
                    (change-records db table (cdr headers) records))
    )
  )        
)

;intoarcea valoare din o lista de perechi 
(define get-value 
  (λ (column pair-list)
    (if (null? pair-list)
        NULL
        (if (equal?  (caar pair-list) column)
            (cdr (car pair-list))
            (get-value column (cdr pair-list))
        )
    )
  )
)


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
;primeste o lista de coloane si o coloana ca si paremetrii si intoarcea indexul coloanei
;am descoperit apoi ca exista functia index-of 
(define get-number-of-column
  (λ (headers column)
    (if (null? headers)
        -1000
        (if (equal? (car headers) column)
            1
            (+ 1 (get-number-of-column (cdr headers) column))
        )
    )
  )
)

;intoarcea intrarile tabelei
(define get-records
  (λ (table)
    (if (null? table)
        '()
        (third table)
  )
))

;intoarcea valorile unei coloane primite ca parametru 
(define get-records-column
  (λ (db table-name column)
    (if (null? (get-table db table-name))
        '()
        (if (equal? (third (get-table db table-name)) '())
            '()
            (last (take (get-records (get-table db table-name)) (get-number-of-column (get-columns (get-table db table-name)) column)))
  ))
)
  )


(define simple-select
  (λ (db table-name columns)
    (let ( (coloane (get-columns (get-table db table-name)))
           (tabel   (get-table db table-name)))
    (if (null? columns)
        '()
         (if (or (null? (get-records tabel)) (> (get-number-of-column coloane (car columns)) (length (get-records tabel) )))
             '()
          (append (list (car (reverse (take (get-records tabel) (get-number-of-column coloane (car columns))))))
               (simple-select db table-name (cdr columns)) )
          ))
     )
     )
)

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

;functia care intoarce o coloana dintr-un anumit se de intrari 
(define get-new-records
  (λ (db table records column)
            (last(take records (get-number-of-column (get-columns table) column)))
            
  ))

;o functie foarte asemanatoare cu simple-select doar aceasta appendeaza o coloana dintr-un anumit se de intrari 
(define modified-simple-select
  (λ (db table columns)
    (let ( (coloane (get-columns table))
           (table-name   (get-name  table)))
    (if (null? columns)
        '()
          (if (pair? (car columns))
              (cons (treat-pair table (car columns)) (modified-simple-select db table (cdr columns)))
              (append (list (get-new-records db table (get-records table) (car columns))) (modified-simple-select db table (cdr columns)))
         )
     )
     )
))

;trateaza cazurile cand coloanelee sunt defapt o pereche 
(define treat-pair
  (λ (table pair)
    (letrec ((record (get-new-records db table (get-records table) (cdr pair))))
        (cond ((equal? (car pair) 'max) (max record))
              ((equal? (car pair) 'min) (min record))
              ((equal? (car pair) 'count) (count record))
              ((equal? (car pair) 'avg) (avg record))
              ((equal? (car pair) 'sort-asc) (sort-asc record))
              ((equal? (car pair) 'sort-desc) (sort-desc record))
              ((equal? (car pair) 'sum) (sum record))
        )
      )
    )
)

;functia principala de care realizeaza selectul
(define select
  (λ (db table-name columns conditions)
    (letrec (
          (antet (list table-name (list (get-columns (get-table db table-name))))) 
          (new_table (append antet (list (filter-table db (get-table db table-name) (get-records (get-table db table-name)) conditions filter-elements)))))
      (modified-simple-select db new_table columns)
    )
))

;functia primeste o lista un index si sterge din lista elementul de la indexul dat ca parametru
(define remove-by-index 
  (λ (l index iter)
    (if (null? l)
        '()
        (if (equal? index iter)
            (remove-by-index (cdr l) index (+ iter 1))
            (append (list (car l)) (remove-by-index (cdr l) index (+ iter 1)))
        )
  )
))

;face filtrarea intrarilor tabelei folosind o anumita functie de criteriu f
(define filter-table
  (λ(db table records conditions f)
  (if (null? conditions)
      records
      (filter-table db table
             (filter-table-condition db table (first (car conditions)) (second (car conditions)) (third (car conditions)) records f)
             (cdr conditions) f)
   ))
)

;functia de filtrare 
(define (filter-elements db table oper column arg records)
  (filter (lambda (elem)  (if (equal? elem NULL) #t (not (oper elem arg)))) (apply list (get-new-records db table records column)))
)

;functie ajutatoare pentru filter-table ce primeste o conditie si face filtrarea dupa conditia respectiva 
(define filter-table-condition
  (λ(db table oper column arg records f)
   (let* ( (bad-elems (f db table oper column arg records))
          (table-name (get-name table))
          (antet (list table-name (list (get-columns (get-table db table-name))))))
     (if (null? bad-elems)
         records
         (if (isMember? 'null (get-new-records db table records column))
             (remove-records db table records column (cons  NULL bad-elems))
             (remove-records db table records column bad-elems)
         )
     )
   )
))

;functie ce sterge din tabela intrarea corespunzatoare unui index
(define remove-from-table
  (λ (db table-name headers records index)
    (let  ( (table (get-table db table-name))
            )
      (if (null? headers)
          '()
          (append (list (remove-by-index (get-new-records db table records (car headers)) index 1))
                  (remove-from-table db table-name (cdr headers) records index))
         )

      )
    )
)

;functia ce sterge intrarile din tabela care au elemente ce intra in bad-elements
(define remove-records
  (λ(db table records column bad-elements)
   (letrec ((name (get-name table))
           (columns (get-columns table)))
     
   (if (null? bad-elements)
       records
       (remove-records db table 
         (remove-from-table db name columns records (get-index-of-elem (car bad-elements) (get-new-records db table records column) ))
          column (cdr bad-elements))) 
   )
))

;determina indexul unui element dintr-o lista
;index-of era din nou foarte folositor
(define get-index-of-elem
  (λ(elem records)
    (if (null? records)
        -100
        (if (equal? elem (car records))
            1
            (+ 1 (get-index-of-elem elem (cdr records)))
        )
    )
  )
)

;functiile ajutatoare pentru cazul in care coloanele date sunt defapt perechi
(define min
  (λ(l)
    (foldl (lambda (x acc) (if (> x acc) acc x)) 10000 l)
  )
)

(define max
  (λ(l)
    (foldl (lambda (x acc) (if (< x acc) acc x)) -10000 l)
  )
)
(define count
  (λ(l)
    (if (null? l)
        0
        (length (remove-duplicates l))
     )
  )
)
(define sum
  (λ(l)
    (if (null?  l)
        0
        (apply + 0 l)
    )   
  )
)
(define avg
  (λ(l)
    (/ (sum l) (length l))
  )
)
(define sort-asc
  (λ(l)
    (sort l  <)
  )
)
(define sort-desc
  (λ(l)
    (sort l >)
  )
)
;===================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
;functia de update propriu-zisa
(define update
  (λ (db table-name values conditions)
    (letrec (
          (table (get-table db table-name))
          (indexes (good-cond db table (get-records table) conditions return-position)))
      (add-table (remove-table db table-name) (updating db table (get-records table) values indexes))
    )
))

;o functie intermediara care putea fi integrata in update dar am ales sa fac asa pentru o mai buna intelegere a codului
(define (updating db table records values indexes)
  (let ((antet (list (get-name table) (list (get-columns (get-table db (get-name table)))))))
    (append antet (list (update-columns db table (get-records table) values indexes)))
  ))

;functia care face update-ul tuturor intrarilor 
(define (update-columns db table records values indexes)
  (if (null? values)
      records
      (update-columns db table (update-column db table records (cdr (car values))
                                              (get-number-of-column (get-columns table) (car (car values))) 1 indexes) (cdr values) indexes)
))

;functia ajutatoare care se ocupa doar de o singura coloana 
(define (update-column db table records elem index-of-column iter indexes)
  (if (null? records)
      '()
      (if (equal? iter index-of-column)
          (append (list (updating-list (car records) elem 1 indexes)) (update-column db table (cdr records) elem index-of-column (+ 1 iter) indexes))
          (append (list (car records)) (update-column db table (cdr records) elem index-of-column (+ 1 iter) indexes))
      )
   )
)

;functie ajutatoare care schimba elementele din lista cu indexul in indexes cu elementul din elem 
(define (updating-list l elem iter indexes)
  (if (null? l)
      '()
      (if (isMember? iter indexes)
          (cons elem (updating-list (cdr l) elem (+ 1 iter) indexes))
          (cons (car l) (updating-list (cdr l) elem (+ 1 iter) indexes))
       )
   )
)



;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
;functia principala de delete
;face o noua tabela si o sterge pe cea veche din baza de 
(define delete
  (λ (db table-name conditions)
    (letrec (
          (antet (list table-name (list (get-columns (get-table db table-name))))) 
          (new_table (append antet (list (remove-lines db (get-table db table-name) (get-records (get-table db table-name)) conditions return-position)))))
      (if (null? conditions)
          (add-table (remove-table db table-name) (create-table table-name (get-columns (get-table db table-name))))
          (add-table (remove-table db table-name) new_table)
  )))
)

;sterge liniile tabelei facund transpunsa acestora
;stergerea se va face pe linii chiar dar eu mi-am retinut la inceput pe coloane 
(define remove-lines
  (λ(db table records conditions func)
    (let ( (res (helper (apply map list records) (good-cond db table records conditions func) 1)) )
      (if (null? res)
          res
          (apply map list res)
      )
  ))
)

(define helper
  (λ(records l iter)
    (if (null? records)
        '()
    (if (isMember? iter l)
        (helper (cdr records) l (+ iter 1))
        (append (list (car records)) (helper (cdr records) l (+ iter 1)))
    )
    )
))


;intoarce pozitia elementelor dintr-o coloana care respecta conditia 
(define return-position
  (λ(db table oper arg iter rec)
    (if (null? rec)
        '()
        (if (oper (car rec) arg)
            (append (list iter) (return-position db table oper arg (+ iter 1) (cdr rec)))
            (return-position db table oper arg (+ iter 1) (cdr rec))
        )
    )
  )
)

;face intersectia listelor cu indicii care respecta conditiile din conditions 
(define good-cond
  (λ(db table records conditions f)
    
     (if (null? conditions)
         (filling-list 1 (+ 1(length (car records))))
         (letrec ((cond (car conditions))
          (oper (first cond))
          (column (second cond))
          (arg (third cond))
          (rec (get-new-records db table records column))
          (new_list (f db table oper arg 1 rec)))
               (inters-list new_list (good-cond db table records (cdr conditions) f) )
           ))
 ))

;intoarce o lista (1 .... len) pentru a ajuta la intersectia listelor
(define (filling-list iter len)
  (if (equal? iter len)
      '()
     (append (list iter) (filling-list (+ iter 1) len))
  )
)

;verifica daca x este in lista l
(define isMember? 
  (λ(x l)
  (if (null? l)
      #f
      (if (equal? (car l ) x)
          #t
          (isMember? x (cdr l))
      )
  )
))

;face intersectia a doua liste
(define (inters-list  l1 l2)
  (if (null? l1)
      '()
      (if (isMember? (car l1) l2)
          (append (list (car l1)) (inters-list (cdr l1) l2))
          (inters-list (cdr l1) l2)
      )
  )
)

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
;merge doar pentru cazul in care avem 2 tabele cu nr de coloane 2 
(define natural-join
  (λ (db tables columns conditions)
    (letrec (
      (table-name1 (car tables))
      (table-name2 (car (cdr tables)))
      (columns1 (intersection-list columns (get-columns (get-table db table-name1))))
      (columns2 (intersection-list columns (get-columns (get-table db table-name2))))
     )
      (merge (modified-select db table-name1  columns1 conditions) (modified-select db table-name2  columns2 conditions))
  ))
)

;determina elementul 2 al unei perechi dintr-o lista de perechi
(define (get-element l1 number)
  (if (null? l1)
      '()
       (if (equal? (first (car l1)) number)
          (second (car l1))
          (get-element (cdr l1) number)
       )
  )
)


(define (func l1 numbers)
  (if (null? numbers)
      '()
       (append (list (get-element l1 (car numbers))) (func l1 (cdr numbers))) 
  )
)

(define (merge l2 l1)
  (append l2 (list (func (apply map list l1) (car l2))))
)
      
;sterge liniile care contin NULL 
(define (improved-filter-elements records)
  (apply map list (refilter (apply map list records)))
)
;face intersectia a 2 liste 
(define (intersection-list all-indexes good-indexes-list )
  (filter (lambda (x) (isMember? x good-indexes-list)) all-indexes)
)
(define (refilter records)
  (if (null? records)
      '()
      (if (isMember? NULL (car records))
          (refilter (cdr records))
          (append (list (car records)) (refilter (cdr records)))
       )
   )
)
(define modified-select
  (λ (db table-name columns conditions)
    (letrec (
          (antet (list table-name (list (get-columns (get-table db table-name))))) 
          (new_table (append antet (list (improved-filter-elements (filter-table db (get-table db table-name) (get-records (get-table db table-name)) conditions filter-elements))))))
      (modified-simple-select db new_table columns)
    )
))