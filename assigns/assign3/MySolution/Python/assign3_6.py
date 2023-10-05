class fnlist:
    ctag = -1
    cons1 = None
    cons2 = None
    
    def get_ctag(self):
        return self.ctag
    
    def get_cons1(self):
        return self.cons1
    
    def get_cons2(self):
        return self.cons2

    def __iter__(self):
        return fnlist_iter(self)

    def __reversed__(self):
        return fnlist_reverse(self)

class fnlist_nil(fnlist):
    ctag = 0

class fnlist_cons(fnlist):
    ctag = 1

    def __init__(self, cons1, cons2):
        self.cons1 = cons1
        self.cons2 = cons2

def fnlist_iter(xs):
    if xs.get_ctag() == 0:
        return iter([])
    elif xs.get_ctag() == 1:
        return iter([xs.get_cons1()] + list(xs.get_cons2()))

def fnlist_reverse(xs):
    if xs.get_ctag() == 0:
        return fnlist_nil()
    elif xs.get_ctag() == 1:
        cons1 = xs.get_cons1()
        cons2 = xs.get_cons2()
        return fnlist_cons(fnlist_reverse(cons2), cons1)