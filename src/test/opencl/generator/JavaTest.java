package opencl.generator;

import arithmetic.interop.jVar;
import ir.Type;
import ir.UndefType$;
import ir.ast.*;
import ir.interop.*;
import lift.arithmetic.Cst;
import lift.arithmetic.StartFromRange;
import lift.arithmetic.Var;
import opencl.executor.Compile;
import opencl.executor.Executor;
import opencl.ir.interop.*;
import opencl.ir.package$;
import opencl.ir.pattern.*;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import scala.collection.JavaConversions;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

public class JavaTest {

    private final UserFun add = package$.MODULE$.add();

    private final UserFun pair = jUserFunDef.create(
            "pair",
            "x",
            "{ Tuple t = {x, x}; return t; }",
            jFloat.getSingleton(),
            jTupleType.create(jFloat.getSingleton(), jFloat.getSingleton()));

    private final UserFun mult = package$.MODULE$.mult();
    private final UserFun plusOne = package$.MODULE$.plusOne();

    private final UserFun neg = package$.MODULE$.neg();
    private final UserFun id = package$.MODULE$.id();
    private final UserFun idFI = package$.MODULE$.idFI();

    private final UserFun distance = jUserFunDef.create("dist", jStringArray.create("x", "y", "a", "b", "id"), "{ Tuple t = {(x - a) * (x - a) + (y - b) * (y - b), id}; return t; }", jTypeArray.create(jFloat.getSingleton(), jFloat.getSingleton(), jFloat.getSingleton(), jFloat.getSingleton(), jInt.getSingleton()), jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()));
    private final UserFun minimum = jUserFunDef.create("minimum", jStringArray.create("x", "y"), "{ return x._0 < y._0 ? x : y; }", jTypeArray.create(jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()), jTupleType.create(jFloat.getSingleton(), jInt.getSingleton())), jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()));
    private final UserFun getSecond = jUserFunDef.create("getSecond", "x", "{ return (float) x._1; }", jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()), jFloat.getSingleton());


    @BeforeClass
    public static void before() {
        Executor.loadLibrary();
        Executor.init();
    }

    @AfterClass
    public static void after() {
        Executor.shutdown();
    }

    @Test
    public void vectorNegSimple() {

        Lambda1 negFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N", new StartFromRange(new Cst(1)))),
                (input) -> jJoin.comp(jfun.create((Param x1) -> {
                    return jMapGlb.create(jfun.create((Param x2) -> {
                        return jMapSeq.create(neg).call(x2);
                    })).call(x1);
                })).comp(jfun.create((Param x) -> jSplit.create(4).call(x))).call(input));

        Compile.apply(negFun);

        Lambda negFun2 = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N", new StartFromRange(new Cst(1)))),
                (input) -> jJoin.comp(jMapGlb.create(
                                jMapSeq.create(neg))
                ).comp(jSplit.create(4)).call(input));

        Compile.apply(negFun2);
    }

    @Test
    public void vectorNegSimpleWithoutJfun() {
        MapGlb mg = jMapGlb.create(neg);

        Type arrayType = jArrayType.create(jFloat.getSingleton(), jVar.create("N", new StartFromRange(new Cst(1))));

        Param p = Param.apply(arrayType);

        Lambda f = Lambda$.MODULE$.apply(new Param[]{p}, mg.call(p));

        Compile.apply(f);
    }

    @Test
    public void vectorScalarMultiplication() {

        Lambda multFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N", new StartFromRange(new Cst(1)))),
                jFloat.getSingleton(),
                (input, alpha) -> jMapGlb.create(
                        jfun.create((x) -> mult.call(alpha, x)))
                        .call(input));

        Compile.apply(multFun);
    }

    @Test
    public void vectorScalarMultiplicationWithoutJfun() {
        Type arrayType = jArrayType.create(jFloat.getSingleton(), jVar.create("N", new StartFromRange(new Cst(1))));
        Type floatType = jFloat.getSingleton();

        Param p0 = Param.apply(arrayType);
        Param p1 = Param.apply(floatType);

        Param[] params = {p0, p1};

        Param undefParam = Param.apply(UndefType$.MODULE$);
        Expr multExpr = mult.apply(JavaConversions.asScalaBuffer(Arrays.asList(p1, undefParam)));
        Lambda1 multLambda = new Lambda1(new Param[]{undefParam}, multExpr);
        MapGlb mg = MapGlb$.MODULE$.apply(multLambda);


        Lambda f = Lambda$.MODULE$.apply(params, mg.apply(JavaConversions.asScalaBuffer(Collections.singletonList(params[0]))));

        Compile.apply(f);
    }

    @Test
    public void kmeans() {
        // kmeans
        Var n = (Var) jVar.create("N", new StartFromRange(new Cst(1)));
        Var k = (Var) jVar.create("K", new StartFromRange(new Cst(1)));



        // @formatter:off
        Lambda fun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jInt.getSingleton(), k),
                (x, y, a, b, i) ->
                        jMapGlb.create(
                                jfun.create(xy ->
                                                jMapSeq.create(getSecond).comp(
                                                        jToGlobal.create(jMapSeq.create(idFI)).comp(jReduceSeq.create(minimum, package$.MODULE$.Tuple2ToValue(new scala.Tuple2<>(java.lang.Float.MAX_VALUE, -1))).comp(
                                                                        jMapSeq.create(jfun.create(abi -> distance.apply(
                                                                                JavaConversions.asScalaBuffer(Arrays.asList(jGet.create(xy, 0), jGet.create(xy, 1),
                                                                                        jGet.create(abi, 0), jGet.create(abi, 1), jGet.create(abi, 2))))))
                                                                )
                                                        )).call(jZip.call(Arrays.asList(a, b, i)))
                                )
                        ).call(jZip.call(x, y))
        );

        Compile.apply(fun);
    }

    @Test
    public void kmeansWithoutJfun() {
        Var n = (Var) jVar.create("N", new StartFromRange(new Cst(1)));
        Var k = (Var) jVar.create("K", new StartFromRange(new Cst(1)));

        Type[] types = {
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jInt.getSingleton(), k)};

        List<Expr> params = Arrays.asList(types).stream().map(Param::apply).collect(Collectors.toList());

        Expr zip3 = jZip.call(params.subList(2, 5));
        Expr zip2 = jZip.call(params.subList(0, 2));
        Param undef1 = Param.apply(UndefType$.MODULE$);
        Param undef2 = Param.apply(UndefType$.MODULE$);

        Expr distExpr = distance.apply(JavaConversions.asScalaBuffer(Arrays.asList(jGet.create(undef1, 0), jGet.create(undef1, 1),
                jGet.create(undef2, 0), jGet.create(undef2, 1), jGet.create(undef2, 2))));

        Lambda1 lmap1 = new Lambda1(new Param[]{undef2}, distExpr);

        MapSeq map1 = MapSeq$.MODULE$.apply(lmap1);


        MapSeq map2 = jMapSeq.create(getSecond);
        Lambda1 reduce = jReduceSeq.create(minimum, package$.MODULE$.Tuple2ToValue(new scala.Tuple2<>(java.lang.Float.MAX_VALUE, -1)));
        toGlobal mapId = jToGlobal.create(jMapSeq.create(idFI));

        Expr f = map2.comp(mapId).comp(reduce).comp(map1).call(zip3);

        Lambda1 l = new Lambda1(new Param[]{undef1}, f);
        MapGlb mg = MapGlb$.MODULE$.apply(l);

        //noinspection SuspiciousToArrayCall
        Lambda function = Lambda$.MODULE$.apply(params.toArray(new Param[params.size()]), mg.call(zip2));

        Compile.apply(function);
    }

    @Test
    public void vectorPair() {

        Lambda pairFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N", new StartFromRange(new Cst(1)))),
                (input) -> jJoin.comp(jMapWrg.create(
                        jJoin.comp(jMapLcl.create(
                                jMapSeq.create(pair)
                        )).comp(jSplit.create(4))
                )).comp(jSplit.create(1024)).call(input)
        );

        Compile.apply(pairFun);
    }

    @Test
    public void matrixMatrix() {

        Var N = (Var) jVar.create("N", new StartFromRange(new Cst(1)));
        Var M = (Var) jVar.create("M", new StartFromRange(new Cst(1)));
        Var K = (Var) jVar.create("K", new StartFromRange(new Cst(1)));


        Type[] types = {
                jArrayType.create((jArrayType.create(jFloat.getSingleton(), K)), N),
                jArrayType.create((jArrayType.create(jFloat.getSingleton(), K)), M)
        };

        List<Expr> params = Arrays.asList(types).stream().map(Param::apply).collect(Collectors.toList());


        Param undef0 = Param.apply();
        Param undef1 = Param.apply();
        Param undef2 = Param.apply();

        Expr multExpr = mult.apply(JavaConversions.asScalaBuffer(Arrays.asList(jGet.create(undef2, 0), jGet.create(undef2, 1))));
        Lambda1 multLambda = new Lambda1(new Param[]{undef2}, multExpr);
        Lambda map = Lambda.FunDefToLambda(jMapSeq.create(multLambda));

        Lambda1 reduce = jReduceSeq.create(add, package$.MODULE$.FloatToValue(0.0f));

        Lambda mapId = Lambda.FunDefToLambda(jToGlobal.create(Lambda.FunDefToLambda(jMapSeq.create(id))));

        Expr zip2 = jZip.call(undef0, undef1);

        Expr f = mapId.comp(reduce.comp(map)).call(zip2);

        Lambda1 l = new Lambda1(new Param[]{undef1}, f);
        Lambda ms = Lambda.FunDefToLambda(jMapSeq.create(l));

        Lambda1 l2 = new Lambda1(new Param[]{undef0}, ms.call(params.get(1)));

        Lambda mg = Lambda.FunDefToLambda(jMapGlb.create(l2));

        //noinspection SuspiciousToArrayCall
        Lambda function = Lambda$.MODULE$.apply(params.toArray(new Param[params.size()]), mg.call(params.get(0)));

        Compile.apply(function);

    }

    @Test
    public void matrixPlusOne() {

        Var M = (Var) jVar.create("M", new StartFromRange(new Cst(1)));
        Var K = (Var) jVar.create("K", new StartFromRange(new Cst(1)));

        Function<Param, Lambda> test = a -> jfun.create((r) -> add.call(r, a));

        Function<Param, Lambda> test2 = a -> jfun.create(row -> jMapSeq.create(test.apply(a)).call(row));

        BiFunction<Param, Param, Expr> test3 = (a, b) -> jMapGlb.create(test2.apply(b)).call(a);

        Lambda f = jfun.create(
                jArrayType.create(jArrayType.create(jFloat.getSingleton(), K), M),
                jFloat.getSingleton(),
                test3
        );

        Compile.apply(f);
    }

    @Test
    public void simpleCompTest() {

        FunDecl simpleComp = neg.comp(plusOne);

        Lambda1 negFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N", new StartFromRange(new Cst(1)))),
                (input) -> jMapGlb.create(simpleComp).call(input));

        Compile.apply(negFun);
    }

    @Test
    public void composeUserFunctionWithPattern() {

        Lambda function = jfun.create(
                jArrayType.create(jArrayType.create(jFloat.getSingleton(), jVar.create("M", new StartFromRange(new Cst(1)))), jVar.create("N", new StartFromRange(new Cst(1)))),
                (input) -> jMapGlb.create(
                        jfun.create(row -> jMapSeq.create(neg).comp(jToGlobal.create(jMapSeq.create(id)).comp(jReduceSeq.create(add, package$.MODULE$.FloatToValue(0.0f)))).call(row))
                ).call(input));

        Compile.apply(function);
    }
}
