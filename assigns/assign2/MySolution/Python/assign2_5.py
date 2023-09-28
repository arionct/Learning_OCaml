def fnlist_make_fwork(fopr):
    def fn(xs):
        acc = []
        for x in xs:
            acc.insert(0, fopr(x))
        return acc
    return fn