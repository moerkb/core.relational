(ns core.relational.examples.sap
  (require [core.relational :refer :all]))

; Supplier and Parts (SAP) database by Chris Date
; Exercises as from lecture by Prof. Dr. Renz 
; (http://homepages.thm.de/~hg11260/dbs.html 4 and 5)
; SQL equivalents are given

(def s (create-relation '[sno sname status city]
                        #{["S1" "Smith" 20 "London"]
                          ["S2" "Jones" 10 "Paris"]
                          ["S3" "Blake" 30 "Paris"]
                          ["S4" "Clark" 20 "London"]
                          ["S5" "Adams" 30 "Athens"]}))

(def sp (create-relation '[sno pno qty]
                         #{["S1" "P1" 300]
                           ["S1" "P2" 200]
                           ["S1" "P3" 400]
                           ["S1" "P4" 200]
                           ["S1" "P5" 100]
                           ["S1" "P6" 100]
                           ["S2" "P1" 300]
                           ["S2" "P2" 400]
                           ["S3" "P2" 200]
                           ["S4" "P2" 200]
                           ["S4" "P4" 300]
                           ["S4" "P5" 400]}))

(def p (create-relation '[pno pname color weight city]
                        #{["P1" "Nut"   "Red"   12 "London"]
                          ["P2" "Bolt"  "Green" 17 "Paris"]
                          ["P3" "Screw" "Blue"  17 "Oslo"]
                          ["P4" "Screw" "Red"   14 "London"]
                          ["P5" "Cam"   "Blue"  12 "Paris"]
                          ["P6" "Cog"   "Red"   19 "London"]}))

stop

; SAP01
; select sno, status from s where city = 'Paris'

; SAP02
; select distinct pno from sp

; SAP03
; select * from s

; SAP04
; select sno from s where city = 'Paris' and status > 20

; SAP05
; select sno, status from s where city = 'Paris' order by status desc

; SAP06
; select pno, pname, weight from p where weight between 16 and 19

; SAP07
; select pno, pname, weight from p where weight in (12, 14, 19)
; or
; select pno, pname, weight from p where weight in (select weight from p
;                                                   where color = 'Red')

; SAP08
; a) select * from s cross join p where s.city = p.city
; b) select * from s natural join sp natural join p where s.city = p.city

; SAP09
; select distinct s.city as 'delivering city', p.city as 'bearing city'
;   from s join sp using (sno) join p using (pno)
; natural join cannot be used here!

; SAP10
; select s1.sno, s2.sno from s as s1 cross join s as s2
;   where s1.city = s2.city and s1.sno < s2.sno

; SAP11
; a) select sname from s natural join sp where sp.pno = 'P2'
; b) select sname from s where sno in (select sno from sp where pno = 'P2')

; SAP12
; select distinct sname from s natural join sp natural join p
;   where p.color = 'Red'

; SAP13
; select count(sno) as Quantity from s

; SAP14
; select count(distinct sno) as Quantity from sp

; SAP15
; select sum(qty) as "Number of part P2" from sp where pno = 'P2'

; SAP16
; select pno, sum(qty) as Quantity from sp group by pno order by pno

; SAP17
; select pno from sp group by pno having count(*) > 1 order by pno

; SAP18
; create table SCopy (
;   sno    char(3) primary key,
;   sname  char(12),
;   status integer,
;   city   char(15))

; SAP19
; alter table SCopy add column postcode char(8)

; SAP20
; create index postcode_ix on SCopy(postcode)

; SAP21
; drop table SCopy

; SAP22
; create table SCopy  as select * from s;
; create table PCopy  as select * from p;
; create table SPCopy as select * from sp;
;
; insert into SCopy(sno, sname, status, city)
;   values('S6', 'Black', 15, 'Rome');
;
; select * from SCopy

; SAP23
; select pno, sum(qty) into Quantity from sp by pno

; SAP24
; update SCopy set status = 50 where sno = 'S2'

; SAP25
; update SPCopy set qty = 10 where sno in (select sno from SCopy where
;                                            city = 'London')

; SAP26
; delete from SCopy where sno = 'S4'

; SAP27
; delete from SPCopy where qty >= 300

; SAP28
; drop table SPCopy;
; drop table SCopy;
; drop table PCopy;
; drop table Quantity;

