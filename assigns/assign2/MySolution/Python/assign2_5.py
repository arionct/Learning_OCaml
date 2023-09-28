def fnlist_make_fwork(fopr):
    def fn(xs):
        acc = []
        for x in xs:
            acc.append(fopr(x))
        return acc
    return fn