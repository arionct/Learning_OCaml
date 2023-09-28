import sys
sys.path.append("./../../../../classlib/Python")

def fnlist_make_fwork(fopr):
  acc = []
  def fn(xs):
    acc.append(xs)
    fopr(fn)
    return acc
  return fn