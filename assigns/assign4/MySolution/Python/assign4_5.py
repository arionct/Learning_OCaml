def string_fset_at(cs, i0, c0):
  return ''.join(c if i != i0 else c0 for i, c in enumerate(cs))

alphabet = ''.join(chr(ord('a') + i) for i in range(26))

def list_of_buddies(word):
  n0 = len(word)
  buddies = []

  for i0 in range(n0):
    c0 = word[i0]
    for c1 in alphabet:
      if c1 != c0:
        new_word = string_fset_at(word, i0, c1)
        buddies.append(new_word)

  return buddies