object live_testing {
  import funsets.FunSets._

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)

  val union_ = union(s1, s2)

  val interseccion = intersect(union_, s2)

  val diferencia = diff(s1, s2)
  singletonSet('')





}
