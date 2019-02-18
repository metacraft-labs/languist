import macros, tables

proc filterTableP[A, B](t: TableRef[A, B], pred: proc(k: A, v: B): bool): TableRef[A, B] =
  result = newTable[A, B]()
  for k, v in t.pairs:
    if pred(k, v):
      result.add(k, v)

template filterTable*[A, B](t: TableRef[A, B], pred: untyped): TableRef[A, B] =
  filterTableP(t, proc(k: A, v: B): bool = (let k{.inject.} = k; let v{.inject.} = v; pred))

proc mapTableTP[A, B, C](t: TableRef[A, B], op: proc(k: A, v: B): (A, C)): TableRef[A, C] =
  result = newTable[A, C](rightSize(t.len))
  for k, v in t.pairs:
    let (rk, rv) = op(k, v)
    result.add(rk, rv)

proc mapTableSP[A, B, C](t: seq[A], op: proc(it: A): (B, C)): TableRef[B, C] =
  result = newTable[B, C](rightSize(t.len))
  for it in t.items:
    let (rk, rv) = op(it)
    result.add(rk, rv)

template mapTable*[A, B](t: TableRef[A, B], op: untyped): untyped =
  mapTableTP(t, proc(k: A, v: B): auto = (let k{.inject.} = k; let v{.inject.} = v; op))

template mapTable*[A](t: seq[A], op: untyped): untyped =
  mapTableSP(t, proc(it: A): auto = (let it{.inject.} = it; op))

macro with*(a: untyped, b: untyped): untyped =
  result = quote:
    var tmp = `a`
    var e: ref Exception
    var t = ""
    try:
      tmp.enter()
      `b`
    except:
      e = getCurrentException()
      t = getStackTrace()
    tmp.exit(e, e, t)
    if not e.isNil:
      raise e

template hasField*(field: untyped): untyped =
  type Accepted = concept a
     a.`field`

  Accepted
