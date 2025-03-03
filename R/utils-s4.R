methods::setClassUnion(
  name = "characterOrNull",
  members = c("character", "NULL")
)

methods::setClassUnion(
  name = "integerOrNull",
  members = c("integer", "NULL")
)

methods::setClassUnion(
  name = "DateOrNull",
  members = c("Date", "NULL")
)
