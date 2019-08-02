# set -eux

tabs -20

o() {
  mkdir -p /tmp/o
  t=$(mktemp -p /tmp/o)
  cat - > "$t"
  echo "$t"
}

# Compile time measurements

mkdir "$(pwd)/bench/out2" >/dev/null 2>&1 && sbt "run -d $(pwd)/bench/out2 bench/jmh-dotty/src/main/scala/1.scala"

for framework in Inlined Shapeless3 Staged; do
  for type in P0 P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
    test -f "bench/$framework-eq-$type.log" ||\
    sbt "dotty-bench-bootstrapped/jmh:run 10 10 1 $(echo "import given Shapeless3.K0._; val x = $framework.Eq.derived[$type]" | o) -classpath $(pwd)/bench/out2 -Xmax-inlines 1000" | tee "bench/$framework-eq-$type.log"
  done
done

for framework in Shapeless3 Staged; do
  for type in PK0 PK1 PK2 PK3 PK4 PK5 PK6 PK7 PK8 PK9 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100 CK1 CK2 CK3 CK4 CK5 CK6 CK7 CK8 CK9 CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100; do
    test -f "bench/$framework-functor-$type.log" ||\
    sbt "dotty-bench-bootstrapped/jmh:run 10 10 1 $(echo "import given Shapeless3.K1._; val x = $framework.Functor.derived[$type]" | o) -classpath $(pwd)/bench/out2 -Xmax-inlines 1000" | tee "bench/$framework-functor-$type.log"
  done
done

for type in PK0 PK1 PK2 PK3 PK4 PK5 PK6 PK7 PK8 PK9 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100 CK1 CK2 CK3 CK4 CK5 CK6 CK7 CK8 CK9 CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100; do
  framework="Hand"
  test -f "bench/$framework-functor-$type.log" ||\
  sbt "dotty-bench-bootstrapped/jmh:run 10 10 1 bench/jmh-dotty/src/main/hand/Functor$type.scala -classpath $(pwd)/bench/out2" | tee "bench/$framework-functor-$type.log"
done

for type in P0 P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
  framework="Hand"
  test -f "bench/$framework-eq-$type.log" ||\
  sbt "dotty-bench-bootstrapped/jmh:run 10 10 1 bench/jmh-dotty/src/main/hand/Eq$type.scala -classpath $(pwd)/bench/out2" | tee "bench/$framework-eq-$type.log"
done

# Runtime measurements

test -f "bench/runtime.log" ||\
(cd "bench/jmh-dotty" && sbt "jmh:run -wi 10 -i 10 -f 1 playground.Benchmarks" | tee "../runtime.log")


# Bytecode size measurements

cmd=""
mkdir -p "bench/outs"

for framework in Inlined Shapeless3 Staged; do
  for type in P0 P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
    mkdir -p "bench/outs/$framework-$type"
    cmd="$cmd ; run $(echo "import given Shapeless3.K0._; val x = $framework.Eq.derived[$type]" | o) -classpath $(pwd)/bench/out2 -Xmax-inlines 1000 -d bench/outs/$framework-$type"
  done
done

for framework in Shapeless3 Staged; do
  for type in PK0 PK1 PK2 PK3 PK4 PK5 PK6 PK7 PK8 PK9 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100 CK1 CK2 CK3 CK4 CK5 CK6 CK7 CK8 CK9 CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100; do
    mkdir -p "bench/outs/$framework-$type"
    cmd="$cmd ; run $(echo "import given Shapeless3.K1._; val x = $framework.Functor.derived[$type]" | o) -classpath $(pwd)/bench/out2 -Xmax-inlines 1000 -d bench/outs/$framework-$type"
  done
done

for h in EqP0 EqP1 EqP2 EqP3 EqP4 EqP5 EqP6 EqP7 EqP8 EqP9 EqP10 EqP20 EqP30 EqP40 EqP50 EqP60 EqP70 EqP80 EqP90 EqP100 EqC1 EqC2 EqC3 EqC4 EqC5 EqC6 EqC7 EqC8 EqC9 EqC10 EqC20 EqC30 EqC40 EqC50 EqC60 EqC70 EqC80 EqC90 EqC100 FunctorPK0 FunctorPK1 FunctorPK2 FunctorPK3 FunctorPK4 FunctorPK5 FunctorPK6 FunctorPK7 FunctorPK8 FunctorPK9 FunctorPK10 FunctorPK20 FunctorPK30 FunctorPK40 FunctorPK50 FunctorPK60 FunctorPK70 FunctorPK80 FunctorPK90 FunctorPK100 FunctorCK1 FunctorCK2 FunctorCK3 FunctorCK4 FunctorCK5 FunctorCK6 FunctorCK7 FunctorCK8 FunctorCK9 FunctorCK10 FunctorCK20 FunctorCK30 FunctorCK40 FunctorCK50 FunctorCK60 FunctorCK70 FunctorCK80 FunctorCK90 FunctorCK100; do
  type=$(echo $h | sed s/Eq// | sed s/Functor//)
  mkdir -p "bench/outs/Hand-$type"
  cmd="$cmd ; run "bench/jmh-dotty/src/main/hand/$h.scala" -classpath $(pwd)/bench/out2 -d bench/outs/Hand-$type"
done

test -f "bench/bytecode-sizes.log" ||\
sbt "$cmd"

rm "bench/bytecode-sizes.log" || true
for framework in Hand Inlined Shapeless3 Staged; do
  for type in P0 P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
    printf "$framework-$type,\t" >> "bench/bytecode-sizes.log"
    { find "bench/outs/$framework-$type" -type f -name "*.class" -printf "%s+"; echo 0; } | bc >> "bench/bytecode-sizes.log"
  done
done

for framework in Hand Shapeless3 Staged; do
  for type in PK0 PK1 PK2 PK3 PK4 PK5 PK6 PK7 PK8 PK9 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100 CK1 CK2 CK3 CK4 CK5 CK6 CK7 CK8 CK9 CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100; do
    printf "$framework-$type,\t" >> "bench/bytecode-sizes.log"
    { find "bench/outs/$framework-$type" -type f -name "*.class" -printf "%s+"; echo 0; } | bc >> "bench/bytecode-sizes.log"
  done
done

echo
echo --
echo Reporting
echo --

echo; echo "Compile time P\tHand\tInlined\tShapeless3\tStaged"

for type in P0 P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P20 P30 P40 P50 P60 P70 P80 P90 P100; do
  printf "$type\t" | sed "s/P//"
  for framework in Hand Inlined Shapeless3 Staged; do
    if test -f "bench/$framework-eq-$type.log"; then
      cat "bench/$framework-eq-$type.log" | grep '±(' | grep -oP '(?<=  ).*(?=±)' | tr -d '\n'
    else printf 0
    fi
    printf "\t"
  done
  echo
done

echo; echo "Compile time C\tHand\tInlined\tShapeless3\tStaged"

for type in C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C20 C30 C40 C50 C60 C70 C80 C90 C100; do
  printf "$type\t" | sed "s/C//"
  for framework in Hand Inlined Shapeless3 Staged; do
    if test -f "bench/$framework-eq-$type.log"; then
      cat "bench/$framework-eq-$type.log" | grep '±(' | grep -oP '(?<=  ).*(?=±)' | tr -d '\n'
    else printf 0
    fi
    printf "\t"
  done
  echo
done

echo; echo "Compile time PK\tHand\tInlined\tShapeless3\tStaged"

for type in PK0 PK1 PK2 PK3 PK4 PK5 PK6 PK7 PK8 PK9 PK10 PK20 PK30 PK40 PK50 PK60 PK70 PK80 PK90 PK100; do
  printf "$type\t" | sed "s/PK//"
  for framework in Hand Inlined Shapeless3 Staged; do
    if test -f "bench/$framework-functor-$type.log"; then
      cat "bench/$framework-functor-$type.log" | grep '±(' | grep -oP '(?<=  ).*(?=±)' | tr -d '\n'
    else printf 0
    fi
    printf "\t"
  done
  echo
done

echo; echo "Compile time CK\tHand\tInlined\tShapeless3\tStaged"

for type in CK1 CK2 CK3 CK4 CK5 CK6 CK7 CK8 CK9 CK10 CK20 CK30 CK40 CK50 CK60 CK70 CK80 CK90 CK100 ; do
  printf "$type\t" | sed "s/CK//"
  for framework in Hand Inlined Shapeless3 Staged; do
    if test -f "bench/$framework-functor-$type.log"; then
      cat "bench/$framework-functor-$type.log" | grep '±(' | grep -oP '(?<=  ).*(?=±)' | tr -d '\n'
    else printf 0
    fi
    printf "\t"
  done
  echo
done

echo; echo "Runtime P\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 0 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/runtime.log" | grep -e 'HandEqP.*thrpt'                    | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'InlinedEqDerivedP.*thrpt'          | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'Shapeless3EqDerivedP.*thrpt'       | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'StagedEqDerivedP.*thrpt'           | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Runtime C\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 1 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/runtime.log" | grep -e 'HandEqC.*thrpt'                    | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'InlinedEqDerivedC.*thrpt'          | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'Shapeless3EqDerivedC.*thrpt'       | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'StagedEqDerivedC.*thrpt'           | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Runtime PK\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 0 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/runtime.log" | grep -e 'handFunctorPK.*thrpt' | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(yes "0" | head -n 20 | o) \
  $(cat "bench/runtime.log" | grep -e 'shapeless3FunctorDerivedPK.*thrpt' | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'stagedFunctorDerivedPK.*thrpt'     | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Runtime CK\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 1 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/runtime.log" | grep -e 'handFunctorCK.*thrpt' | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(yes "0" | head -n 19 | o) \
  $(cat "bench/runtime.log" | grep -e 'shapeless3FunctorDerivedCK.*thrpt' | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o) \
  $(cat "bench/runtime.log" | grep -e 'stagedFunctorDerivedCK.*thrpt'     | tr -s ' ' | cut -d ' ' -f 5 |\
                                                         sed 's-^-scale=3; -' | sed 's-$-/1000000.0-' | bc -l | o)

echo; echo "Bytecode P\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 0 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Hand-P[0-9].*,'       | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Inlined-P[0-9].*,'    | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-P[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-P[0-9].*,'     | grep -oe '[0-9]*$' | o)

echo; echo "Bytecode C\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 1 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Hand-C[0-9].*,'       | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Inlined-C[0-9].*,'    | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-C[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-C[0-9].*,'     | grep -oe '[0-9]*$' | o)

echo; echo "Bytecode PK\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 0 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Hand-PK[0-9].*,'       | grep -oe '[0-9]*$' | o) \
  $(yes "0" | head -n 20 | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-PK[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-PK[0-9].*,'     | grep -oe '[0-9]*$' | o)

echo; echo "Bytecode CK\tHand\tInlined\tShapeless3\tStaged"
paste \
  $(cat $(seq 1 9 | o) $(seq 10 10 100 | o)| o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Hand-CK[0-9].*,'       | grep -oe '[0-9]*$' | o) \
  $(yes "0" | head -n 19 | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Shapeless3-CK[0-9].*,' | grep -oe '[0-9]*$' | o) \
  $(cat "bench/bytecode-sizes.log" | grep -e 'Staged-CK[0-9].*,'     | grep -oe '[0-9]*$' | o)
