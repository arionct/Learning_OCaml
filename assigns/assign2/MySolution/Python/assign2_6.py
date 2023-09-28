import sys

sys.path.append("./../../../../classlib/Python")

def string_merge(cs1, cs2):
    n1 = len(cs1)
    n2 = len(cs2)

    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                c1 = cs1[i1]
                c2 = cs2[i2]
                if c1 <= c2:
                    work(c1)
                    foreach(i1 + 1, i2, work)
                else:
                    work(c2)
                    foreach(i1, i2 + 1, work)
            else:
                for i in range(n1 - i1):
                    work(cs1[i1 + i])
        else:
            for i in range(n2 - i2):
                work(cs2[i2 + i])

    return ''.join(foreach(0, 0, lambda x: [x]))

def string_length(cs):
    return len(cs)

def string_get_at(cs, i):
    return cs[i]

def string_make_fwork(foreach):
    def fn(cs):
        return ''.join(foreach(i, lambda x: x) for i in cs)
    return fn
