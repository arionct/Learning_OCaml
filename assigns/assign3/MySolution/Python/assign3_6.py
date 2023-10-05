from MyPython import *

class mylist:
    ctag = -1
    def get_ctag(self):
        return self.ctag

class mylist_nil(mylist):
    def __init__(self):
        self.ctag = 0

class mylist_cons(mylist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons1
        self.cons2 = cons2

    def get_cons1(self):
        return self.cons1

    def get_cons2(self):
        return self.cons2

class mylist_snoc(mylist):
    def __init__(self, xs, x1):
        self.ctag = 2
        self.xs = xs
        self.x1 = x1

    def get_xs(self):
        return self.xs

    def get_x1(self):
        return self.x1

class mylist_reverse(mylist):
    def __init__(self, xs):
        self.ctag = 3
        self.xs = xs

    def get_xs(self):
        return self.xs

class mylist_append2(mylist):
    def __init__(self, xs1, xs2):
        self.ctag = 4
        self.xs1 = xs1
        self.xs2 = xs2

    def get_xs1(self):
        return self.xs1

    def get_xs2(self):
        return self.xs2

def mylist_foreach(xs, work_func):
    if xs.ctag == 0:  # MyNil
        return None
    elif xs.ctag == 1:  # MyCons
        work_func(xs.get_cons1())
        mylist_foreach(xs.get_cons2(), work_func)
    elif xs.ctag == 2:  # MySnoc
        mylist_foreach(xs.get_xs(), work_func)
        work_func(xs.get_x1())
    elif xs.ctag == 3:  # MyReverse
        mylist_rforeach(xs.get_xs(), work_func)
    elif xs.ctag == 4:  # MyAppend2
        mylist_foreach(xs.get_xs1(), work_func)
        mylist_foreach(xs.get_xs2(), work_func)

def mylist_rforeach(xs, work_func):
    if xs.ctag == 0:  # MyNil
        return None
    elif xs.ctag == 1:  # MyCons
        mylist_rforeach(xs.get_cons2(), work_func)
        work_func(xs.get_cons1())
    elif xs.ctag == 2:  # MySnoc
        work_func(xs.get_x1())
        mylist_rforeach(xs.get_xs(), work_func)
    elif xs.ctag == 3:  # MyReverse
        mylist_foreach(xs.get_xs(), work_func)
    elif xs.ctag == 4:  # MyAppend2
        mylist_rforeach(xs.get_xs2(), work_func)
        mylist_rforeach(xs.get_xs1(), work_func)
