class Form:
    def __init__(self, gen, size=10):
        self.data = [gen() for _ in range(size)]
        self.val = sum(map(int, self.data))
    
    def __int__(self):
        return self.val

    def max_abs_val(self):
        c_max = max(map(abs, map(int, self.data)))
        return max(self.val, c_max)
    
    def __str__(self):
        return '(+ ' + ' '.join(map(str, self.data)) + ')'


import random as r

def n_gen(min=-4,max=4):
    return lambda: r.randint(min, max)

def form_gen(depth):
    if depth <= 1:
        return lambda: Form(n_gen())
    return lambda: Form(form_gen(depth - r.randint(1, 3)))

big_boy = Form(form_gen(4))
print(big_boy)
print(int(big_boy), big_boy.max_abs_val())