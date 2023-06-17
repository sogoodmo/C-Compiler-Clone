def ans(faces):
    fnc = lambda x, y: 2 if x + y == 7 else 1 
    return min(sum(fnc(t, f) for f in faces if f != t) for t in range(1, 7))

faces = [1, 2, 6, 3]
print(ans(faces))