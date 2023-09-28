import sys
sys.path.append("./../../../../classlib/Python")

def fnlist_make_fwork(fopr):
  #acc = []
  def fn(xs):
    newxs = fopr(xs)
    #acc.append(newxs)
    #fopr(xs)
    return newxs
  return fn


def double(x):
    return 2 * x

mylist = [1, 2, 3, 4, 5]
myfn = fnlist_make_fwork(double)
result = myfn(mylist)
print(result)  # Output: [2, 4, 6, 8, 10]
