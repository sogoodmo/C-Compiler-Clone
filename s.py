res = [] 
with open('text.txt', 'r') as f:
    for line in f:
        res.append(line[:-1])
    res[-1] = res[-1] + 'e'
with open("out.txt", 'w') as o:
    for line in res:
        o.write(f'static void {"_".join(word.title() for word in line.split("_"))}() {{}};\n')