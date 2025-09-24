from sympy import symbols, Matrix, det, sqrt  # 导入 sqrt 而非使用 math

# 正确定义符号变量为 p1-p6
p1, p2, p3, p4, p5, p6 = symbols('p1 p2 p3 p4 p5 p6')

# 定义函数表达式（使用 SymPy 的 sqrt 和正确的幂运算符 **）
f1 = sqrt((p4 - p2)**2 + (p5 - p3)**2 + p6**2)
f2 = sqrt(p4**2 + p5**2 + p6**2)
f3 = sqrt((p4 - p1)**2 + p5**2 + p6**2)
f4 = sqrt(p2**2 + p3**2)
f5 = p1
f6 = sqrt((p2 - p1)**2 + p3**2)

# 构建雅可比矩阵
F = Matrix([f1, f2, f3, f4, f5, f6])
vars = Matrix([p1, p2, p3, p4, p5, p6])
J = F.jacobian(vars)

# 计算行列式（注意：6x6符号行列式计算可能非常耗时！）
det_J = det(J)
print(det_J)  # 输出或进一步简化