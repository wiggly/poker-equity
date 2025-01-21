
jar:
	sbt "rootJVM / assembly"

exe:
	sbt "rootNative / nativeLink"

run:
	sbt "rootJVM / runMain Main"

benchmark:
	sbt "rootJVM / runMain Benchmark"

profile-benchmark:
	java -agentpath:/home/wiggly/opt/asprof/lib/libasyncProfiler.so=start,event=cpu,file=benchmark.html -cp /home/wiggly/src/poker-equity/jvm/target/scala-3.3.0/wpe.jar Benchmark

profile-main:
	java -agentpath:/home/wiggly/opt/asprof/lib/libasyncProfiler.so=start,event=cpu,file=main.html -cp /home/wiggly/src/poker-equity/jvm/target/scala-3.3.0/wpe.jar Main

    jvm-run:
	java -cp /home/wiggly/src/poker-equity/jvm/target/scala-3.3.0/wpe.jar Main

native-run:
	./native/target/scala-3.3.0/poker-equity-out
