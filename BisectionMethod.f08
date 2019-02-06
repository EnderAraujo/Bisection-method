PROGRAM BisectionMethod
! Algorithm 2.1
! Burden, R. L., & Faires, J. D. (2018). Numerical analysis. Cengage Learning, 8 Ed.
IMPLICIT NONE

INTEGER, PARAMETER :: minIter   =  0 
INTEGER, PARAMETER :: maxIter   = 30 
REAL,    PARAMETER :: Tolerancy =  1E-5

INTEGER :: Iter 
REAL :: Func   
REAL :: x     
REAL :: aFunc 
REAL :: a     
REAL :: bFunc  
REAL :: b      
REAL :: mFunc 
REAL :: m    

a  =  .5
b  = 1.5

Iter  = minIter
aFunc = Func(a)

DO

    m     = a + .5*(b - a)
    mFunc = Func(m)

    IF ( (mFunc == 0) .OR. ((.5*(b - a)) < Tolerancy) ) THEN
        m = m; EXIT
    END IF

    Iter = Iter + 1

    IF (aFunc*mFunc > 0) THEN
        a     = m
        aFunc = mFunc
    ELSE
        b     = m
    END IF
    
END DO

PRINT 120, Iter + 1, m
120 FORMAT (/, 15X, "Exercise 2.1.9.b.   p(", I2, ") =", 1X, F10.8, /)

END PROGRAM BisectionMethod

REAL FUNCTION Func(x)
IMPLICIT NONE
REAL, INTENT(IN) :: x

Func = cos(exp(x) - 2) - exp(x) + 2

END FUNCTION
