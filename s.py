import sympy as sym
x,y = sym.symbols('x,y')
eq1 = sym.Eq(x+y,5)
eq2 = sym.Eq(x**2+y**2,17)
result = sym.solve([eq1,eq2],(x,y))
print(result)