#lang racket

;Hemuppgift 2


;Nedan finns en sekvens av uttryck.
;Vad kommer scheme att skriva som svar på inmatning av dessa uttryck?
;Antag att de matas in i den ordning de står.

(- 9 1) ;beräknar 9-1 och skriver ut resultatet, 8.

(/ 6 2) ;beräknar 6/2 och skriver ut resultatet, 3.

(+ (* 2 4) (- 4 6)) ;beräknar uttrycket (2*4)+(4-6) och utskrift blir 6.

(define a 3) ;vi ger variabeln a värdet 3.

(define b (+ a 1)) ;b ges värdet a+1, alltså värdet 4 då a har värdet 3.

(+ a b (* a b)) ;beräknar a+b+(a*b) vilket med insatta värden ger svaret 19.

(= a b) ;jämför om a och b har samma värde. Returnerar #f eftersom de ej har det.

(cond ((= a 4) 6)         
      ((= b 4) (+ 6 7 a)) 
      (else 25))          
;jämför först a med värdet 4 och returnerar 6 om så är fallet. Annars gå vidare.
;sedan jämförs b med 4 och returnerar värdet av (6+7+a)=16. Annars gå vidare.
;om inget av ovanstående villkor uppfyllts, returnera talet 25.
;I detta fall skrivs talet 16 ut, eftersom jämförelsen mellan b och 4 uppfylls.

(* (cond ((> a b) a) 
         ((< a b) b) 
         (else -1))   
   (+ a 1))           
;är a större än b? returnera a, annars gå vidare till rad 2
;är b större än a? returnera b, annars gå vidare till rad 3
;gå vidare med värdet -1 om inget av villkoren ovan stämmer
;multiplicerar värdet vi fick ut av cond-satsen med a+1=4 och skriv sedan ut.
;här blir utskriften 16,
;villkoret (< a b) uppfylls och värdet b=4 multipliceras med värdet a+1=4.