package types

def assertRange(range:Range):Unit =
  if range.start > range.end then assert(false,"range.start > range.end")
  if !range.isInclusive && range.start == range.end then assert(false,"not inclusive range and range.start == range.end")

