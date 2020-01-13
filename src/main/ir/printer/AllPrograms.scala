package ir.printer

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._
import opencl.ir.abs

object AllPrograms{

val k = SizeVar("K")
val M = SizeVar("M")
val P = SizeVar("P")
val kSize = SizeVar("K")
val K = SizeVar("K")
val C = SizeVar("C")
val xSize = SizeVar("X")
val N = SizeVar("N")
val F = SizeVar("F")

val featuresType    = ArrayType(ArrayType(Float, P), F)
val clustersType    = ArrayType(ArrayType(Float, F), C)

val currentDistance = UserFun("currentDistance", Array("x", "y"),
    "{ return (x - y) * (x - y); }",
    Seq(Float, Float), Float)
val test = UserFun("test", Array("dist", "tuple"),
  "{" +
    "float min_dist = tuple._0;" +
    "int i          = tuple._1;" +
    "int index      = tuple._2;" +
    "if (dist < min_dist) {" +
    "  Tuple t = {dist, i + 1, i};" +
    "  return t;" +
    "} else {" +
    "  Tuple t = {min_dist, i + 1, index};" +
    "  return t;" +
    "}" +
    "}",
  Seq(Float, TupleType(Float, Int, Int)), TupleType(Float, Int, Int))
val distance = UserFun("distance_", Array("loc", "lat", "lng"),
    "{ return sqrt( (lat - loc._0) * (lat - loc._0) + (lng - loc._1) * (lng - loc._1) ); }",
    Seq(TupleType(Float, Float), Float, Float), Float)
val mapFun = UserFun("mapFun",
  Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
  """{
    |    #define PIx2 6.2831853071795864769252867665590058f
    |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
    |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
    |    return  bla;
    |}""".stripMargin,
  Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))
val reduceFun = UserFun("reduceFun",
  Array("x", "y"),
  """{
        | x._0 += y._0;
        | x._1 += y._1;
        | return x;
      }""".stripMargin,
  Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))
val blackScholesComp =
  UserFun("blackScholesComp", "inRand",
    """|{
       |  #define S_LOWER_LIMIT 10.0f
       |  #define S_UPPER_LIMIT 100.0f
       |  #define K_LOWER_LIMIT 10.0f
       |  #define K_UPPER_LIMIT 100.0f
       |  #define T_LOWER_LIMIT 1.0f
       |  #define T_UPPER_LIMIT 10.0f
       |  #define R_LOWER_LIMIT 0.01f
       |  #define R_UPPER_LIMIT 0.05f
       |  #define SIGMA_LOWER_LIMIT 0.01f
       |  #define SIGMA_UPPER_LIMIT 0.10f
       |  Tuple p;
       |
       |  float S = S_LOWER_LIMIT * inRand + S_UPPER_LIMIT * (1.0f - inRand);
       |  float K = K_LOWER_LIMIT * inRand + K_UPPER_LIMIT * (1.0f - inRand);
       |  float T = T_LOWER_LIMIT * inRand + T_UPPER_LIMIT * (1.0f - inRand);
       |  float R = R_LOWER_LIMIT * inRand + R_UPPER_LIMIT * (1.0f - inRand);
       |  float V = SIGMA_LOWER_LIMIT * inRand + SIGMA_UPPER_LIMIT * (1.0f - inRand);
       |
       |  float sqrtT = sqrt(T);
       |  float d1 = (log(S / K) + ((R + V * V * 0.05f) * T)) / V * sqrtT;
       |  float d2 = d1 - (V * sqrtT);
       |
       |  float CNDD1;
       |  {
       |    float L;
       |    float K1;
       |    float w;
       |    float a1 = 0.319381530f;
       |    float a2 = -0.356563782f;
       |    float a3 = 1.781477937f;
       |    float a4 = -1.821255978f;
       |    float a5 = 1.330274429f;
       |    float a6 = 2.506628273f;
       |    L = fabs(d1);
       |    K1 = 1.0f / (1.0f + 0.2316419f * L);
       |    w = 1.0f - 1.0f / 1 * a6 * exp((-1 * L) * L / 2) * (a1 * K1 + a2 * K1 * K1 * 1 + a3 * K1 * K1 * K1 * +a4 * K1 * K1 * K1 * K1 * 1 + a5 * K1 * K1 * K1 * K1 * K1);
       |    if (d1 < 0) {
       |      CNDD1 = 1.0f - w;
       |    } else {
       |      CNDD1 = w;
       |    }
       |  }
       |  float CNDD2;
       |  {
       |    float L;
       |    float K2;
       |    float w;
       |    float a1 = 0.319381530f;
       |    float a2 = -0.356563782f;
       |    float a3 = 1.781477937f;
       |    float a4 = -1.821255978f;
       |    float a5 = 1.330274429f;
       |    float a6 = 2.506628273f;
       |    L = fabs(d2);
       |    K2 = 1.0f / (1.0f + 0.2316419f * L);
       |    w = 1.0f - 1.0f / 1 * a6 * exp((-1 * L) * L / 2) * (a1 * K2 + a2 * K2 * K2 * 1 + a3 * K2 * K2 * K2 * +a4 * K2 * K2 * K2 * K2 * 1 + a5 * K2 * K2 * K2 * K2 * K2);
       |    if (d2 < 0) {
       |      CNDD2 = 1.0f - w;
       |    } else {
       |      CNDD2 = w;
       |    }
       |  }
       |  float expRT = exp(-T * R);
       |  Tuple result;
       |  result._0 = S * CNDD1 - K * expRT * CNDD2;
       |  result._1 = K * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1);
       |  return result;
       |}
      """.stripMargin
    , Float, TupleType(Float, Float))
val update =
UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
    """|{
    |  float4 newPos;
    |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
    |  newPos.w = pos.w;
    |  float4 newVel;
    |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
    |  newVel.w = vel.w;
    |  Tuple t = {newPos, newVel};
    |  return t;
    |}
    """.stripMargin,
    Seq(Float4, Float4, Float, Float4), TupleType(Float4, Float4))
val calcAccNoAdd =
UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"),
    """|{
    |  float4 r;
    |  r.xyz = p2.xyz - p1.xyz ;
    |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
    |  float invDist = 1.0f / sqrt(distSqr + espSqr);
    |  float invDistCube = invDist * invDist * invDist;
    |  float s = invDistCube * p2.w;
    |  float4 res;
    |  res.xyz = s * r.xyz;
    |  return res;
    |}
    | """.stripMargin,
    Seq(Float4, Float4, Float, Float), Float4)
val md = UserFun("md", Array("i", "j", "niters", "size"),
  """|{
     |  const float space = 2.0f / size;
     |  float Zr = 0.0f;
     |  float Zi = 0.0f;
     |  float Cr = (j * space - 1.5f);
     |  float Ci = (i * space - 1.0f);
     |  int y = 0;
     |
     |  for (y = 0; y < niters; y++) {
     |    const float ZiN = Zi * Zi;
     |    const float ZrN = Zr * Zr;
     |    if(ZiN + ZrN > 4.0f) break;
     |    Zi *= Zr;
     |    Zi *= 2.0f;
     |    Zi += Ci;
     |    Zr = ZrN - ZiN + Cr;
     |  }
     |  return ((y * 255) / niters);
     |}
     |""".stripMargin, Seq(Int, Int, Int, Int), Int)
val select = UserFun("select_", Array("tuple"),
    "{ return tuple._2; }",
    Seq(TupleType(Float, Int, Int)), Int)
val phiMag = UserFun("phiMag",
                     Array("phiR", "phiI"),
                     "{ return phiR * phiR + phiI * phiI; }",
                     Seq(Float, Float),
                     Float)
val mdCompute = UserFun("updateF",
  Array("f", "ipos", "jpos", "cutsq", "lj1", "lj2"),
  """|{
     |  // Calculate distance
     |  float delx = ipos.x - jpos.x;
     |  float dely = ipos.y - jpos.y;
     |  float delz = ipos.z - jpos.z;
     |  float r2inv = delx*delx + dely*dely + delz*delz;
     |  // If distance is less than cutoff, calculate force
     |  if (r2inv < cutsq) {
     |    r2inv = 1.0f/r2inv;
     |    float r6inv = r2inv * r2inv * r2inv;
     |    float forceC = r2inv*r6inv*(lj1*r6inv - lj2);
     |    f.x += delx * forceC;
     |    f.y += dely * forceC;
     |    f.z += delz * forceC;
     |  }
     |  return f;
     |}
  """.stripMargin,
  Seq(Float4, Float4, Float4, Float, Float, Float),
  Float4)

def mvAlpha = fun(
  ArrayType(ArrayType(Float, K), N),
  ArrayType(Float, K),
  Float,
  (matrix, vector, alpha) =>
    Join() o
      Map(fun(row =>
        Map(fun(x => mult(x, alpha))) o
          Reduce(add, 0.0f) o Map(fun(y => mult(y._0, y._1))) $ Zip(row, vector)
      )) $ matrix
)
def vecAdd = fun(
  ArrayType(Float, K),
  ArrayType(Float, K),
  (a,b) => Map(fun(x => add(x._0, x._1))) $ Zip(a, b)
)
val gemvN = fun(
  ArrayType(ArrayType(Float, M), N),
  ArrayType(Float, M),
  ArrayType(Float, N),
  Float,
  Float,
  (matrix, vectorX, vectorY, alpha, beta) => {
    Map(fun(t =>
      Map(fun(x =>
        add(
          mult(x, alpha),
          mult(Get(t, 1), beta)
        )
      )) o
        Reduce(add, 0.0f) o
        Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(vectorX, Get(t, 0))
    )) $ Zip(matrix, vectorY)
  })

val mmNN = fun(
  ArrayType(ArrayType(Float, K), M),
  ArrayType(ArrayType(Float, N), K),
  (A, B) => {
    Map(fun(aRow =>
      Map(fun(bCol =>
        Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
      )) o Transpose() $ B
    )) $ A
  })

val dot = fun(
  ArrayType(Float, N),
  input =>
    Reduce(add, 0.0f) o Map(abs) $ input
)

val mmNT = fun(
  ArrayType(ArrayType(Float, K), M),
  ArrayType(ArrayType(Float, K), N),
  (A, B) => {
    Map(fun(aRow =>
      Map(fun(bCol =>
        Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
      )) $ B
    )) $ A
  })

val nbody = fun(
    ArrayType(Float4, N),
    ArrayType(Float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
    Map(fun(p1 =>
        Map(fun(acceleration =>
            update(Get(p1, 0), Get(p1, 1), deltaT, acceleration)
            )) o Reduce(VectorizeUserFun(4, add), Value("0.0f", Float4)
            ) o Map(\(p2 =>
              calcAccNoAdd(Get(p1,0), p2, deltaT, espSqr)
              )) $ pos
      )) $ Zip(pos, vel)
)

val gesummvNN = fun(
  ArrayType(ArrayType(Float, K), N),
  ArrayType(ArrayType(Float, K), N),
  ArrayType(Float, K),
  Float,
  Float,
  (A, B, x, alpha, beta) =>
    vecAdd(mvAlpha(A, x, alpha), mvAlpha(B, x, beta))
)

val gemvT = fun(
  ArrayType(ArrayType(Float, N), M),
  ArrayType(Float, M),
  ArrayType(Float, N),
  Float,
  Float,
  (matrix, vectorX, vectorY, alpha, beta) => {
    Map(fun(t =>
      Map(fun(x =>
        add(
          mult(x, alpha),
          mult(Get(t, 1), beta)
        )
      )) o
        Reduce(add, 0.0f) o
        Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(vectorX, Get(t, 0))
    )) $ Zip(Transpose() $ matrix, vectorY)
  })

val molecularDynamics = fun(
  ArrayTypeWSWC(Float4, N),
  ArrayTypeWSWC(ArrayTypeWSWC(Int, N), M),
  Float,
  Float,
  Float,
  (particles, neighbourIds, cutsq, lj1, lj2) =>
    Zip(particles, Transpose() $ neighbourIds) :>>
      Map( \(p =>
        Filter(particles, p._1) :>>
          ReduceSeq(\((force, n) =>
            mdCompute(force, p._0, n, cutsq, lj1, lj2)
          ), Value(0.0f, Float4))
      ) )
)

val mriqComputeQ = fun(
    ArrayType(Float, xSize),
    ArrayType(Float, xSize),
    ArrayType(Float, xSize),
    ArrayType(TupleType(Float, Float, Float, Float), kSize),
    (x, y, z, kValues) =>
      Map(\(t =>
        Reduce(reduceFun, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))) o
          Map(\(k => mapFun(t._0, t._1, t._2, k._0, k._1, k._2, k._3))) $ kValues
      )) $ Zip(x, y, z)
  )

val mandelbrot = fun(
  ArrayType(Int, SizeVar("N")),
  Int,
  Int,
  (in, niters, size) => Map(fun(i => Map(fun(j => md(i, j, niters, size))) $ in)) $ in
)

val blackScholes = fun(
  ArrayTypeWSWC(Float, SizeVar("N")),
  inRand => Map(blackScholesComp) $ inRand
)

val mriqPhiMag = fun(
  ArrayTypeWSWC(Float, k),
  ArrayTypeWSWC(Float, k),
  (phiR, phiI) => Map(\(x => phiMag(x._0, x._1))) $ Zip(phiR, phiI)
)

val asum = fun(
  ArrayType(Float, N),
  Float,
  (vector, alpha) =>
    Map(fun(x => mult(x, alpha))) $ vector
)

val mv = fun(
  ArrayType(ArrayType(Float, K), N),
  ArrayType(Float, K),
  (matrix, vector) =>
    Map(fun(row =>
        Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(row, vector)
    )) $ matrix
)

val nearestNeighbour = fun(
  ArrayTypeWSWC(TupleType(Float, Float), N), Float, Float,
  (locations, lat, lng) => {
    locations :>> Map( \(loc => distance(loc, lat, lng)) )
  })

val mmTT = fun(
  ArrayType(ArrayType(Float, M), K),
  ArrayType(ArrayType(Float, K), N),
  (A, B) => {
    Map(fun(aRow =>
      Map(fun(bCol =>
        Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
      )) $ B
    )) o Transpose() $ A
  })

val scal = fun(
  ArrayType(Float, N),
  Float,
  (vector, alpha) =>
    Map(fun(x => mult(x, alpha))) $ vector
)

val mvAsMM = fun(
  ArrayType(ArrayType(Float, K), N),
  ArrayType(ArrayType(Float, 1), K), // Column vector
  (matrix, vector) =>
    Map(fun(row =>
      Map( fun( col =>
        Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(row, col)
      )) o Transpose() $ vector
    )) $ matrix
)

val kmeans = fun(
    featuresType, clustersType,
    (features, clusters) =>
    Map( \( feature =>
      Map(Map(select)) o
      ReduceSeq( \( (tuple, cluster) =>
        Map(\((x) => test(x._0,x._1)))
      $ Zip(Reduce(add, 0.0f) o
        Map(fun(x => currentDistance(x._0, x._1))) $ Zip(feature, cluster), tuple)
      ), Value("{3.40282347e+38, 0, 0}", ArrayType(TupleType(Float, Int, Int),1))
   ) $ clusters
)) o Transpose() $ features
)

val gesummvTT = fun(
  ArrayType(ArrayType(Float, N), K),
  ArrayType(ArrayType(Float, N), K),
  ArrayType(Float, K),
  Float,
  Float,
  (A, B, x, alpha, beta) =>
    vecAdd(mvAlpha(Transpose() $ A, x, alpha), mvAlpha(Transpose() $ B, x, beta))
)

val mmTN = fun(
  ArrayType(ArrayType(Float, M), K),
  ArrayType(ArrayType(Float, N), K),
  (A, B) => {
    Map(fun(aRow =>
      Map(fun(bCol =>
        Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
      )) o Transpose() $ B
    )) o Transpose() $ A
  })

}