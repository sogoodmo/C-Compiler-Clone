import functools 
def do_twice(func):
    functools.wraps(func)
    def wrapper_do_twice(*args, **kwargs):
        print("I am top wrapper")
        func(*args, **kwargs)
        func(*args, **kwargs)
        print("I am bottom wrapper") 
    
    return wrapper_do_twice

@do_twice
def greet_person(name):
    print(f'hi {name}') 

@do_twice
def greet_generak():
    print('hi')
      
greet_generak()
greet_person('mo')
