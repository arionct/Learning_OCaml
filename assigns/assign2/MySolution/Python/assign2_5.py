import sys
sys.path.append("./../../../../classlib/Python")

def fnlist_make_fwork(fopr):
  acc = []
  def fn(xs):
    acc.insert(0, xs)
  fopr(fn)
  return acc