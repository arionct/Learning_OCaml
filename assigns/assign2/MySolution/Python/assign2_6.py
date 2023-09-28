import sys
sys.path.append("./../../../../classlib/Python")

def string_merge(cs1, cs2):
    n1 = string_length(cs1)
    n2 = string_length(cs2)
    
    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                c1 = string_get_at(cs1, i1)
                c2 = string_get_at(cs2, i2)
                if c1 <= c2:
                    work(c1)
                    foreach(i1 + 1, i2, work)
                else:
                    work(c2)
                    foreach(i1, i2 + 1, work)
            else:
                for iter in range(i1, n1):
                    work(cs1[iter])
        else:
            for iter in range(i2, n2):
                work(cs2[iter])

    return ''.join(foreach(0, 0, lambda x: [x]))

def string_length(cs):
    return len(cs)

def string_get_at(cs, i):
    return cs[i]

def string_make_fwork(foreach):
    xs = []
    def charwork(c):
        xs.append(c)
    foreach(charwork)
    return ''.join(xs)
