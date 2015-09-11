def example1():
    a = 1
    b = a

    def someFunc(x):
        """ Always return an integer """
        from random import randint
        return randint(1, 2)*x

    c = someFunc(a)
    d = someFunc(b)

    if c == d:
        print("c and d are the same!")
    else:
        print("c and d are _not_ the same!")


def example2():
    a = {'a' : 1, 'b': 2}
    b = a

    def someFunc(x):
        """ Always return an integer """
        from random import randint
        k = tuple(x.keys())[0]
        del x[k]
        return randint(1, 10)

    c = someFunc(a)
    d = someFunc(b)

    if a == {'a': 1, 'b': 2}:
        print("a is {'a': 1, 'b': 2}")
    else:
        print("a is not the original dictionary")
        print("a is: {}".format(a))


###################################################
## Attempting to create referential transparency ##
###################################################

from copy import deepcopy

def donttouchmyinputs(f):
    """ Passes a deep copy of the inputs to a function. Does not work
        on recursively defined structures (ex: recursive dicts) """

    def wrap(*args, **kwargs):

        new_args = deepcopy(args)
        new_kwargs = deepcopy(kwargs)

        return f(*new_args, **new_kwargs)

    return wrap


def example3():
    a = {'a' : 1, 'b': 2}
    b = a

    @donttouchmyinputs
    def someFunc(x):
        """ Always return an integer """
        from random import randint
        k = tuple(x.keys())[0]
        del x[k]
        return randint(1, 10)

    c = someFunc(a)
    d = someFunc(b)

    if a == {'a': 1, 'b': 2}:
        print("a is {'a': 1, 'b': 2}")
    else:
        print("a is not the original dictionary")
        print("a is: {}".format(a))


#####################
## Print to stdout ##
#####################

def printFuncName(f):

    def wrap(*args, **kwargs):
        s = "Running function {}".format(f.__name__)
        l = "-" * len(s)

        print(s)
        print(l)

        r = f(*args, **kwargs)

        print()
        return r


    return wrap


if __name__ == "__main__":

    printFuncName(example1)()
    printFuncName(example2)()
    printFuncName(example3)()
