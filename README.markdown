Problem: You are given an arithmetic expression containing N real numbers and N - 1 operators, each either + or *. Your goal is to perform the operations in an order that maximizes the value of the expression. 
         That is, insert N - 1 pairs of parentheses into the expression so that its value is maximized.

For example, 
	1)  For the expression 6 * 3 + 2 * 5, the optimal ordering is to add the middle numbers first, then perform the multiplications: ((6 * (3 + 2))  * 5) = 150. 
	2)  For the expression 0.1 * 0.1 + 0.1, the optimal ordering is to perform the multiplication first, then the addition: ((0.1 * 0.1) + 0.1) = 0.11. 
	3)  For the expression (-3) * 3 + 3, the optimal ordering is ((-3) * 3) + 3) = -6.