module BigInteger (
 fromString,
 toString,
 bigAdd,
 karatsuba,
 checkInput,
) where


fromString ::  [Char] -> [Int]
fromString s = map (\x -> read [x]::Int) s
 

toString :: [Int] -> [Char]
toString [] = ""
toString [x] = (show x)
toString (x:xs) = (show x) ++ toString xs


bigAdd :: [Int] -> [Int] -> Int -> [Int]
bigAdd x y b = longAdd (reverse x) (reverse y) 0 [] b

karatsuba :: [Int] -> [Int] -> Int -> [Int]
karatsuba x y b = toDigits (karatsuba' x y b)

checkInput :: [Int] -> Int -> Bool
checkInput [] b = True
checkInput (x:xs) b = if x>=0 && x<b
                      then checkInput xs b
                      else error "Invalid_Input_Exception"
  



fromDigits xs = sum (zipWith (*) (reverse xs) (iterate (*10) 1))
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++[x `mod` 10]


longAdd [] [] carry ls b= if carry>0
                        then carry:ls
                        else ls
longAdd (y:ys) [] carry ls b= if carry>0
                            then longAdd ys [] ((carry+y)`div`b) (((carry+y)`rem`b):ls) b
                            else longAdd ys [] 0 (y:ls) b
longAdd [] (y:ys) carry ls b= if carry>0
                            then longAdd [] ys ((carry+y)`div`b) (((carry+y)`rem`b):ls) b
                            else longAdd [] ys 0 (y:ls) b
longAdd (x:xs) (y:ys) carry ls b= if (x+y+carry)>=b
                                 then longAdd xs ys ((x+y+carry)`div`b) (((x+y+carry)`rem`b):ls) b 
                                 else longAdd xs ys 0 (((x+y+carry)`rem`b):ls) b

longSub [] [] borrow ls b=ls
longSub (x:xs) [] borrow ls b = if (x-borrow)<0
                                then longSub xs [] 1 ((b+x-borrow):ls) b
                                else longSub xs [] 0 ((x-borrow):ls) b
longSub (x:xs) (y:ys) borrow ls b= if ((x-y-borrow)<0)
                                  then longSub xs ys 1 ((b+x-y-borrow):ls) b
                                  else longSub xs ys 0 ((x-y-borrow):ls) b





karatsuba' x y b
 |(fromDigits x) <b = ((fromDigits x)*(fromDigits y))
 |(fromDigits y) <b = ((fromDigits x)*(fromDigits y))
 |otherwise = let 
               z0= karatsuba' xr yr b
               z1= karatsuba' (longAdd (reverse xl) (reverse xr) 0 [] b) (longAdd (reverse yl) (reverse yr) 0 [] b) b
               z2= karatsuba' xl yl b
              in
               fromDigits (bigAdd (bigAdd (toDigits (z2*10^(((digits`div`2)+(digits`rem`2))*2))) (toDigits ((fromDigits (longSub (reverse (longSub (reverse (toDigits z1)) (reverse (toDigits z2)) 0 [] b)) (reverse (toDigits z0)) 0 [] b))*10^((digits`div`2)+(digits`rem`2)))) b) (toDigits z0) b)
              where 
               digits = max (length x) (length y)
               (xl,xr) = mysplit x ((length x) -((digits`div`2)+(digits`rem`2))) 0
               (yl,yr) = mysplit y ((length y) -((digits`div`2)+(digits`rem`2))) 0



mysplit [] n i = ([],[])
mysplit (x:xs) n i =if i < n
                then let
                      (p1,p2) =  mysplit xs n (i+1)
                     in
                      (x:p1,p2)
                else let
                      (p1,p2) =  mysplit xs n (i+1)
                     in
                      (p1,x:p2)

 