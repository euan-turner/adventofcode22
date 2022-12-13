
def format(file):
  ls = []
  for line in file:
    if line == "\n":
      continue 
    else:
      ls.append(eval(line))
  return ls

def pairUp(packets: list[list[int]]) -> list[tuple[int]]:
  ps = zip(packets[::2], packets[1::2])
  return ps 

def compare(left, right):
  if (type(left) == int and type(right) == int):
    if left < right:
      return 1
    elif left > right:
      return -1
    else:
      return 0
  elif (type(left) == list and type(right) == list):
    for i in range(min(len(left), len(right))):
      comp = compare(left[i], right[i])
      if (comp != 0):
        return comp
    if (len(left) < len(right)):
      return 1
    elif (len(right) < len(left)):
      return -1
    else:
      return 0
  elif (type(right) == list):
    return compare([left], right)
  elif (type(left) == list):
    return compare(left, [right])

def inRightOrder(pairs: list[tuple[int]]) -> list[int]:
  s = 0
  inds = []
  for i, p in enumerate(pairs):
    if compare(*p) == 1:
      s += i + 1
      inds.append(i)
  print(inds)
  print(s)

def partOne():
  file = open("./Day13/packets.txt")
  form = format(file)
  pairs = pairUp(form)
  inRightOrder(pairs)

def partTwo():
  file = open("./Day13/packets.txt")
  form = format(file)
  s, e = [[2]], [[6]]
  S, E = 1, 2
  for f in form:
    if compare(s, f) == -1:
      S += 1
    if compare(e, f) == -1:
      E += 1
  print(S*E)

  ## Need to sort list using compare

if __name__ == "__main__":
  partTwo()