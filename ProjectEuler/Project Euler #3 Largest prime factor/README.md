The basic idea here is to divide the original number

  N = p_1^e_1 p_2^e_2 ... p_k^e_k

prime by prime, until its largest prime factor is left over.

Alas we have no primes available, so we try to divide by

  2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21 ..

To make the time limits it is necessary to stop the divisor trials if
 
   D*D > X
  
because then it is already clear that X can not be split in two factors
and thus X must be prime.

The code can be simplified it is a bit too careful.
