#lang racket
; Milan Kovacs
; Student ID: 21308128

;;             Problem Description:
#| Largest Prime number finder within a number |#
#| Suppose we have a number n, I want to find the largest prime number within n. |#
#| The Mod function will help to compare remainders while checking if a number is divisible by other than 1 and itself |#


(define mod (lambda (x y) ( if (< x 0)
                               (+ x y)
                               ( mod (- x y) y ) ) ) ) ; My own modulus Function



;; =================================== Data used and filtering ========================================================= ;;
(define MyList (list  60134 1414 1555 'oko 1345 'a 151 121 -8 2 1/3 5+3i 5 7 13 21 9267  4332  9432  4635  6657  2917 6487955 4287544 3157440 7980252 9625057 1783115 1831232 7666880 6200083 2150915 ))
(define getnum (lambda (x)
                 (if (integer? x)
                     (if (positive? x)
                         x
                         #f)
                     #f))) ; Testing condition for positive, integer nums. 
(define integerList (lambda (i) (filter getnum i))) ; This also works:(define integerList (lambda (i) (filter identity (map getnum i))))  
(define updatedList (integerList MyList)) ; List of numbers of Positive Integers. 
;; ===================================================================================================================== ;;


;; =================================== Cool Functions ========================================================================== ;;
#|
 - This is the main (idea) function, it returns True or false based on the number entered. I have arranged the called functions from up to down.
   isPrime? will take an argument, other than 1, 2, 3 or any even numver, it will pass it to the function called Check, which creates an array of #t or #f elements, according to
   the number being a divisor of the original number. That is what the for/and function does. I have talked about it in my post, the theory behind it.
   So it will loop through the odd numbers upto and including the OddSqrt of the number. Applies the AND oparator to the list and return #t if they are all true,
   else returns #f (as you would expect.

 - The OddSqrt function takes a number as argument, applies Sqrt, rounds it up via Ceiling, and checks if its odd?. Since the for/and acts like a for loop/while conditional loop,
   I have to increase it by 1 or 2 according to it being even or odd. So ie: the for/and condition being 3 7 2, where 3 is starting variable, <7 is the condition and
   2 is the incrementation. So the variable i, will return the followings: 3 5 and not 7, which is why I have to return 8 as an argument via OddSqrt.

 - The check function will pass the odd numbers into the 'testing' function. It takes the original number, and checks if it evenly divides it. Returns #f #t accordingly. Note: it is #f #t because we want it to not divide for it to be prime number

 - isOdd? is used in my main function. Since we want to find the largest prime number, we can avoid 
|#

(define isPrime? (lambda (number) ; The definition of Prime Numbers: A number that is divisible by 1 and itself. Mathematicians used to treat 1 as a prime as it fit the definition. In the end, mathematicians decided to exclude 1 due to it's uniqueness (ironcically (not hinting at something)). When mathematicians came up with equations to represent numbers in Unique Factorization or The Fundamental Theorem of Arithmetics (it states that every positive whole number can be represent as a unique product of prime), they always had to specify *excluding 1* because it didn't make the number any more unique. IE: 6 could be represented in the product of primes of 2x3 but also 1x2x3 but also 1x1x2x3 but... it is No longer Unique, so 1 was decided to be excluded from the definition of primes. This is why this condition is necessary.
                   (if (= number 1)
                       #f
                       (if (= number 2) ; This condition is necessary because my function down below used to check if a number is odd or even. Generally every even number is NOT PRIME with the exception of 2, as it is divisible by 1 and itself, 2.
                           #t
                           (if (= number 3) ; This condition is only necessary because of my check function and the way it works. It takes 3 as starting and stop if the number is >= OddSqrt, and my OddSqrt function passes in 4 as the limit parameter, thus it will check if ( / 3 3) and it returns #f, so I had to overcome it via this statememt.
                               #t
                               (if (odd? number) ; If the number entered is not 1, 2 or 3 then check if its odd, otherwise return #f
                                   (check number) ;If it is odd, here is where the fun becomes: Go to check function and pass the number there
                                   #f))))))

(define check (lambda (y)
                (if (= (OddSqrt y) 2)
                    2
                    (for/and([i (in-range  3 (OddSqrt y) 2)])   ; Here we are making a for/and list, with the variable i starting at 3 and ending at the OddSqrt of the number passed through with the increments of 2. This will make sure it finishes on an odd number.
                      (testing y i) ) ) ))    ; Apply testing function to the number and for every odd number below and including the oddsqrt

(define OddSqrt (lambda (num)
                  ( if (odd? (ceiling  (sqrt num)))
                       (+ (ceiling  (sqrt num)) 1)
                       (+ (ceiling  (sqrt num)) 2 )))) ;Due to have the for in-range function works, this function appears weird at first glance. So first we are checking if the square root, ceiled up is odd or not. If it is odd, we just want to make sure to check it upto it. If it is even then add 1 and check that. HOWEVER because the for [in-range x y z] takes the x y z parameters as x being starting point, y BEING the WHILE <Y function and z the incrementation, I had to add +1 to both odd and even sides, which is why +1 and +2 appear here. 

(define testing (lambda (Number Divisor)
                  (if (= (mod Number Divisor) 0)
                      #f
                      #t))) ; Testing checks if it has a remainder of 0, and returns #f if it is NOT prime because it has no remainder and #t if doesn't have remainder (Kind of confusing because of the negated logic but it makes sense). These datas are passed back to the for/and list and applies the AND operand and looks for the first #f in it. Otherwise returns #t WHICH MEANS it is a prime number. The fucntion found no divisor in it.



(define isOdd? (lambda (n) (if (odd? n) ;This one will just check if the argument passed by the user is odd or not. Then check the number if it IS odd return it otherwise return the number below it.
                               n
                               (- n 1)))) 
 
;; ======================================================================================================================================= ;;

;; =================================== Main ====================================================== ;;
(define largest(lambda (x)  (if (<= x 2)                      ; Default is 2 if it is less then or equal to two, 1 is not a prime, it will check 0 and -1 and it will raise an error
                                    2
                                    (if (isPrime? (isOdd? x)) ; This will check if the number entered is even or odd and return the closest odd number to it, then check if it's prime
                                        (if (even? x)
                                            (- x 1)           ; If it is Prime, make sure to NOT return the even number, because the previous function was checking the closest ODD number, so X is still even
                                            x)                
                                        (largest (- x 2)))))) ; If the isPrime? test fails, check the next number. Recursion occurs.
;; =============================================================================================== ;;

;; ================================= Run ======================================================== ;;
; This section will run when the file is ran :)
MyList
updatedList
(map largest updatedList)

;; =============================================================================================== ;;

; Thank you for reading my essay. Only cool people make it to the end:  ( ͡° ͜ʖ ͡°) ☕